(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

(* Multicore is based on several layers of monad transformers. The module as a whole is made to provide a monad to explore in parallel different possibilites, with a notion of priority. *)

(* Add a notion of faillibility to the evaluation. "Transformer without module functor" style. *)
type 'a t = (('a, Bug.t) result, Thread.t) State_monad.t

(* ================================================
   Functions to operate on the three monads layers.
   ================================================ *)

let[@inline] return x : _ t = State_monad.return (Ok x)

let[@inline] lift x =
  let ( let+ ) = State_monad.( let+ ) in
  let+ x in
  Ok x

let[@inline] bind (mx : _ t) f : _ t =
  let ( let* ) = State_monad.( let* ) in
  let* mx in
  match mx with Ok x -> f x | Error _ as mx -> State_monad.return mx

let[@inline] ( let* ) mx f = bind mx f

let[@inline] map mx f =
  let ( let+ ) = State_monad.( let+ ) in
  let+ mx in
  match mx with Ok x -> Ok (f x) | Error _ as mx -> mx

let[@inline] ( let+ ) mx f = map mx f

let[@inline] lift_schedulable (v : 'a Scheduler.Schedulable.t) : 'a t =
  let v = State_monad.lift v in
  lift v

let[@inline] with_thread (f : Thread.t -> 'a) : 'a t =
  let x = State_monad.with_state (fun st -> (f st, st)) in
  lift x

let thread = with_thread Fun.id

let[@inline] modify_thread f = lift (State_monad.modify_state f)

let[@inline] set_thread st = modify_thread (Fun.const st)

let solver_to_use = ref None

let solver_dls_key =
  Domain.DLS.new_key (fun () ->
    let solver_to_use = !solver_to_use in
    match solver_to_use with
    | None -> assert false
    | Some solver_to_use -> Solver.fresh solver_to_use () )

let[@inline] solver () = Domain.DLS.get solver_dls_key

let[@inline] choose a b = State_monad.liftF2 Scheduler.Schedulable.choose a b

let[@inline] yield prio = lift_schedulable @@ Scheduler.Schedulable.yield prio

let stop = lift_schedulable Scheduler.Schedulable.stop

(* ============================================
   Now this is actual symbolic execution stuff!
   ============================================ *)

let run exploration_strategy ~workers solver t thread ~at_worker_value
  ~at_worker_init ~at_worker_end =
  solver_to_use := Some solver;
  let module M =
    ( val Symbolic_parameters.Exploration_strategy.to_work_ds_module
            exploration_strategy )
  in
  let module Scheduler = Scheduler.Make (M) in
  let sched = Scheduler.init () in
  Scheduler.add_init_task sched (Fun.const @@ State_monad.run t thread);
  if workers > 1 then Logs_threaded.enable ();
  Array.init workers (fun _i ->
    Scheduler.spawn_worker sched ~at_worker_value ~at_worker_init
      ~finally:at_worker_end )

let add_pc (c : Symbolic_boolean.t) =
  let c = Symbolic_boolean.to_expr c in
  let c = Smtml.Expr.simplify c in
  match Smtml.Expr.view c with
  | Val True -> return ()
  | Val False -> stop
  | _ ->
    let* thread in
    let c = Symbolic_boolean.of_expr c in
    let new_thread = Thread.add_pc thread c in
    set_thread new_thread
[@@inline]

let get_pc () =
  let+ thread in
  let pc = thread.pc in
  let pc = Symbolic_path_condition.slice pc in
  List.fold_left Smtml.Expr.Set.union Smtml.Expr.Set.empty pc

let add_breadcrumb crumb =
  modify_thread (fun t -> Thread.add_breadcrumb t crumb)

let add_label label = modify_thread (fun t -> Thread.add_label t label)

let open_scope scope = modify_thread (fun t -> Thread.open_scope t scope)

let close_scope () = modify_thread (fun t -> Thread.close_scope t)

let with_new_invisible_symbol ty f =
  let* thread in
  let n = thread.num_symbols in
  let+ () = modify_thread Thread.incr_num_symbols in
  let sym = Fmt.kstr (Smtml.Symbol.make ty) "symbol_invisible_%i" n in
  f sym

let with_new_symbol ty f =
  let* thread in
  let n = thread.num_symbols in
  let sym = Fmt.kstr (Smtml.Symbol.make ty) "symbol_%d" n in
  let+ () =
    modify_thread (fun thread ->
      let thread = Thread.add_symbol thread sym in
      Thread.incr_num_symbols thread )
  in
  f sym

(* Yielding is currently done each time the solver is about to be called, in check_reachability and get_model.
  *)
let check_reachability v prio =
  let* () = yield prio in
  let* thread in
  let solver = solver () in
  let pc = thread.pc |> Symbolic_path_condition.slice_on_condition v in
  let stats = thread.bench_stats in
  let reachability =
    Benchmark.handle_time_span stats.solver_sat_time @@ fun () ->
    Solver.check solver pc
  in
  return reachability

let get_model_or_stop symbol =
  (* TODO: better prio here! *)
  let* () = yield Prio.dummy in
  let solver = solver () in
  let* thread in
  let set = thread.pc |> Symbolic_path_condition.slice_on_symbol symbol in
  let stats = thread.bench_stats in
  let symbol_scopes = Symbol_scope.of_symbol symbol in
  let sat_model =
    Benchmark.handle_time_span stats.solver_intermediate_model_time (fun () ->
      Solver.model_of_set solver ~symbol_scopes ~set )
  in
  match sat_model with
  | `Unsat -> stop
  | `Model model -> begin
    match Smtml.Model.evaluate model symbol with
    | Some v -> return v
    | None ->
      (* the model exists so the symbol should evaluate *)
      assert false
  end
  | `Unknown ->
    (* It can happen when the solver is interrupted *)
    (* TODO: once https://github.com/formalsec/smtml/pull/479 is merged
               if solver was interrupted then stop else assert false *)
    stop

let select_inner ~explore_first ?(with_breadcrumbs = true)
  ~check_only_true_branch (cond : Symbolic_boolean.t) ~prio_true ~prio_false =
  let cond = Symbolic_boolean.to_expr cond in
  let cond = Smtml.Expr.simplify cond in
  match Smtml.Expr.view cond with
  | Val True -> return true
  | Val False -> return false
  | _ ->
    let is_other_branch_unsat = Atomic.make false in
    let branch condition final_value prio =
      let* () = add_pc condition in
      let* () =
        if with_breadcrumbs then add_breadcrumb (if final_value then 1 else 0)
        else return ()
      in
      (* this is an optimisation under the assumption that the PC is always SAT (i.e. we are performing eager pruning), in such a case, when a branch is unsat, we don't have to check the reachability of the other's branch negation, because it is always going to be SAT. *)
      if Atomic.get is_other_branch_unsat then begin
        Log.debug (fun m ->
          m "The SMT call for the %b branch was optimized away" final_value );
        (* the other branch is unsat, we must be SAT and don't need to check reachability! *)
        return final_value
      end
      else begin
        (* the other branch is SAT (or we haven't computed it yet), so we have to check reachability *)
        let* satisfiability = check_reachability condition prio in
        begin match satisfiability with
        | `Sat -> return final_value
        | `Unsat ->
          Atomic.set is_other_branch_unsat true;
          stop
        | `Unknown ->
          (* It can happen when the solver is interrupted *)
          (* TODO: once https://github.com/formalsec/smtml/pull/479 is merged
                     if solver was interrupted then stop else assert false *)
          stop
        end
      end
    in

    let cond = Symbolic_boolean.of_expr cond in

    let true_branch = branch cond true prio_true in

    if check_only_true_branch then true_branch
    else
      let false_branch = branch (Symbolic_boolean.not cond) false prio_false in
      let* thread in
      Thread.incr_path_count thread;
      if explore_first then choose true_branch false_branch
      else choose false_branch true_branch
[@@inline]

let select (cond : Symbolic_boolean.t) ~prio_true ~prio_false =
  select_inner cond ~explore_first:true ~prio_true ~prio_false
    ~check_only_true_branch:false
[@@inline]

let summary_symbol (e : Smtml.Expr.t) =
  let* thread in
  match Smtml.Expr.view e with
  | Symbol sym -> return (None, sym)
  | _ ->
    let num_symbols = thread.num_symbols in
    let+ () = modify_thread Thread.incr_num_symbols in
    let sym_name = Fmt.str "choice_i32_%i" num_symbols in
    let sym_type = Smtml.Ty.Ty_bitv 32 in
    let sym = Smtml.Symbol.make sym_type sym_name in
    let assign = Smtml.Expr.(relop Ty_bool Eq (symbol sym) e) in
    (Some assign, sym)

let select_i32 (i : Symbolic_i32.t) =
  let i = Smtml.Expr.simplify i in
  match Smtml.Expr.view i with
  | Val (Bitv bv) when Smtml.Bitvector.numbits bv <= 32 ->
    return (Smtml.Bitvector.to_int32 bv)
  | _ ->
    let* assign, symbol = summary_symbol i in
    let* () =
      match assign with
      | Some assign ->
        let assign = Symbolic_boolean.of_expr assign in
        add_pc assign
      | None -> return ()
    in
    let rec generator () =
      let* possible_value = get_model_or_stop symbol in
      let i =
        match possible_value with
        | Smtml.Value.Bitv bv when Smtml.Bitvector.numbits bv <= 32 ->
          Smtml.Bitvector.to_int32 bv
        | _ ->
          (* it should be a value! *)
          assert false
      in
      let s = Smtml.Expr.symbol symbol in
      (* TODO: everything which follows look like select_inner and could probably be simplified by calling it directly! *)
      let this_value_cond =
        let open Smtml.Expr in
        Bitv.I32.(s = v i) |> Symbolic_boolean.of_expr
      in
      let not_this_value_cond = Symbolic_boolean.not this_value_cond in
      let this_val_branch =
        let* () = add_breadcrumb (Int32.to_int i) in
        let+ () = add_pc this_value_cond in
        i
      in
      let not_this_val_branch =
        let* () = add_pc not_this_value_cond in
        generator ()
      in
      let* thread in
      Thread.incr_path_count thread;
      choose this_val_branch not_this_val_branch
    in
    generator ()

let trap t =
  let* thread in
  let solver = solver () in
  let path_condition = thread.pc in
  let stats = thread.bench_stats in
  let* model =
    Benchmark.handle_time_span stats.solver_final_model_time @@ fun () ->
    match Solver.model_of_path_condition solver ~path_condition with
    | Some model -> return model
    | None ->
      (* It can happen when the solver is interrupted *)
      (* TODO: once https://github.com/formalsec/smtml/pull/479 is merged
             if solver was interrupted then stop else assert false *)
      stop
  in
  State_monad.return (Error { Bug.kind = `Trap t; model; thread })

let assertion_fail c model =
  let* thread in
  State_monad.return (Error { Bug.kind = `Assertion c; model; thread })

let assertion (c : Symbolic_boolean.t) =
  (* TODO: better prio here *)
  let prio_false = Prio.dummy in
  let prio_true = Prio.dummy in
  let* assertion_true =
    select_inner c ~with_breadcrumbs:false ~explore_first:false ~prio_true
      ~prio_false ~check_only_true_branch:false
  in
  if assertion_true then return ()
  else
    let* thread in
    let solver = solver () in
    let path_condition = thread.pc in
    let stats = thread.bench_stats in
    let* model =
      Benchmark.handle_time_span stats.solver_final_model_time @@ fun () ->
      match Solver.model_of_path_condition solver ~path_condition with
      | Some model -> return model
      | None ->
        (* It can happen when the solver is interrupted *)
        (* TODO: once https://github.com/formalsec/smtml/pull/479 is merged
               if solver was interrupted then stop else assert false *)
        stop
    in
    assertion_fail c model

let ite (c : Symbolic_boolean.t) ~(if_true : Symbolic_value.t)
  ~(if_false : Symbolic_value.t) : Symbolic_value.t t =
  match (if_true, if_false) with
  | I32 if_true, I32 if_false ->
    return (Symbolic_value.I32 (Symbolic_boolean.ite c ~if_true ~if_false))
  | I64 if_true, I64 if_false ->
    return (Symbolic_value.I64 (Symbolic_boolean.ite c ~if_true ~if_false))
  | F32 if_true, F32 if_false ->
    return (Symbolic_value.F32 (Symbolic_boolean.ite c ~if_true ~if_false))
  | F64 if_true, F64 if_false ->
    return (Symbolic_value.F64 (Symbolic_boolean.ite c ~if_true ~if_false))
  | Ref _, Ref _ ->
    (* TODO: better prio here *)
    let prio_false = Prio.dummy in
    let prio_true = Prio.dummy in
    let+ b = select c ~prio_true ~prio_false in
    if b then if_true else if_false
  | _, _ -> assert false

let depth () =
  let+ thread in
  thread.depth

let assume c instr_counter =
  (* TODO: better prio here *)
  let* thread in
  let depth = thread.depth in
  let prio_true = Prio.v ~depth ~instr_counter ~distance_to_unreachable:None in
  let prio_false = Prio.low in
  let* assertion_true =
    select_inner c ~with_breadcrumbs:false ~explore_first:true ~prio_true
      ~prio_false ~check_only_true_branch:true
  in
  if assertion_true then return () else stop
