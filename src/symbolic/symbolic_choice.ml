(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

(* Multicore is based on several layers of monad transformers defined in submodules. The module as a whole is made to provide a monad to explore in parallel different possibilites, with a notion of priority. *)

module Eval = struct
  (* Add a notion of faillibility to the evaluation. "Transformer without module functor" style. *)
  module M = State_monad

  type ('a, 's) t = ('a Sym_eval.t, 's) M.t

  let[@inline] return x : _ t = M.return (Sym_eval.EVal x)

  let[@inline] lift x =
    let ( let+ ) = M.( let+ ) in
    let+ x in
    Sym_eval.EVal x

  let[@inline] bind (mx : _ t) f : _ t =
    let ( let* ) = M.( let* ) in
    let* mx in
    match mx with EVal x -> f x | EError _ as mx -> M.return mx

  let ( let* ) = bind

  let[@inline] map mx f =
    let ( let+ ) = M.( let+ ) in
    let+ mx in
    match mx with
    | Sym_eval.EVal x -> Sym_eval.EVal (f x)
    | EError _ as mx -> mx

  let ( let+ ) = map
end

(* the two following functions can not be in the Make function because it breaks with the two instanciation of `Symbolic_choice.Make (Thread_{with,withou}_memory) *)
let solver_to_use = ref None

let solver_dls_key =
  Domain.DLS.new_key (fun () ->
    let solver_to_use = !solver_to_use in
    match solver_to_use with
    | None -> assert false
    | Some solver_to_use -> Solver.fresh solver_to_use () )

(* The core implementation of the monad. It is isolated in a module to restict its exposed interface and maintain its invariant. In particular, choose must guarantee that the Thread.t is cloned in each branch. Using functions defined here should be foolproof. *)
module Make (Thread : Thread_intf.S) = struct
  type 'a t = ('a, Thread.t) Eval.t

  (*
       Here we define functions to seamlessly
       operate on the three monads layers
    *)

  let ( let* ) = Eval.( let* )

  let ( let+ ) = Eval.( let+ )

  let return = Eval.return

  let bind = Eval.bind

  let map = Eval.map

  let lift_schedulable (v : 'a Scheduler.Schedulable.t) : 'a t =
    let v = State_monad.lift v in
    Eval.lift v

  let with_thread (f : Thread.t -> 'a) : 'a t =
    let x = State_monad.with_state (fun st -> (f st, st)) in
    Eval.lift x

  let thread = with_thread Fun.id

  let modify_thread f = Eval.lift (State_monad.modify_state f)

  let set_thread st = modify_thread (Fun.const st)

  let solver () = Domain.DLS.get solver_dls_key

  let choose a b =
    (* Here we are doing an optimization: we can clone only one of the two thread because the clone should not have a dependency on the previous state, it is thus safe to re-use it for the other thread. We choose to clone `b` rather than `a` because `a` is likely to be the first one executed. *)
    let* thread in
    let new_thread = Thread.clone thread in
    let b =
      let* () = modify_thread (fun (_ : Thread.t) -> new_thread) in
      b
    in
    State_monad.liftF2 Scheduler.Schedulable.choose a b

  let yield prio = lift_schedulable @@ Scheduler.Schedulable.yield prio

  let stop = lift_schedulable Scheduler.Schedulable.stop

  type 'a run_result = ('a Sym_eval.t * Thread.t) Seq.t

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
    | Val True -> Eval.return ()
    | Val False -> stop
    | _ ->
      let* thread in
      let c = Symbolic_boolean.of_expr c in
      let new_thread = Thread.add_pc thread c in
      set_thread new_thread
  [@@inline]

  let get_pc () =
    let+ thread in
    let pc = Thread.pc thread in
    let pc = Symbolic_path_condition.slice pc in
    List.fold_left Smtml.Expr.Set.union Smtml.Expr.Set.empty pc

  let add_breadcrumb crumb =
    modify_thread (fun t -> Thread.add_breadcrumb t crumb)

  let add_label label = modify_thread (fun t -> Thread.add_label t label)

  let open_scope scope = modify_thread (fun t -> Thread.open_scope t scope)

  let close_scope () = modify_thread (fun t -> Thread.close_scope t)

  let with_new_invisible_symbol ty f =
    let* thread in
    let n = Thread.num_symbols thread in
    let+ () = modify_thread Thread.incr_num_symbols in
    let sym = Fmt.kstr (Smtml.Symbol.make ty) "symbol_invisible_%i" n in
    f sym

  let with_new_symbol ty f =
    let* thread in
    let n = Thread.num_symbols thread in
    let sym = Fmt.kstr (Smtml.Symbol.make ty) "symbol_%d" n in
    let+ () =
      modify_thread (fun thread ->
        let thread = Thread.add_symbol thread sym in
        Thread.incr_num_symbols thread )
    in
    f sym

  (*
    Yielding is currently done each time the solver is about to be called,
    in check_reachability and get_model.
  *)
  let check_reachability v prio =
    let* () = yield prio in
    let* thread in
    let solver = solver () in
    let pc = Thread.pc thread |> Symbolic_path_condition.slice_on_condition v in
    let stats = Thread.bench_stats thread in
    let reachability =
      Benchmark.handle_time_span stats.solver_sat_time @@ fun () ->
      Solver.check solver pc
    in
    Eval.return reachability

  let get_model_or_stop symbol =
    (* TODO: better prio here! *)
    let* () = yield Prio.dummy in
    let solver = solver () in
    let* thread in
    let set =
      Thread.pc thread |> Symbolic_path_condition.slice_on_symbol symbol
    in
    let stats = Thread.bench_stats thread in
    let symbol_scopes = Symbol_scope.of_symbol symbol in
    let sat_model =
      Benchmark.handle_time_span stats.solver_intermediate_model_time (fun () ->
        Solver.model_of_set solver ~symbol_scopes ~set )
    in
    match sat_model with
    | `Unsat -> stop
    | `Model model -> begin
      match Smtml.Model.evaluate model symbol with
      | Some v -> Eval.return v
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
    | Val True -> Eval.return true
    | Val False -> Eval.return false
    | Val (Bitv _bv) -> Fmt.failwith "unreachable (type error)"
    | _ ->
      let is_other_branch_unsat = Atomic.make false in
      let branch condition final_value prio =
        let* () = add_pc condition in
        let* () =
          if with_breadcrumbs then add_breadcrumb (if final_value then 1 else 0)
          else Eval.return ()
        in
        (* this is an optimisation under the assumption that the PC is always SAT (i.e. we are performing eager pruning), in such a case, when a branch is unsat, we don't have to check the reachability of the other's branch negation, because it is always going to be SAT. *)
        if Atomic.get is_other_branch_unsat then begin
          Log.debug (fun m ->
            m "The SMT call for the %b branch was optimized away" final_value );
          (* the other branch is unsat, we must be SAT and don't need to check reachability! *)
          Eval.return final_value
        end
        else begin
          (* the other branch is SAT (or we haven't computed it yet), so we have to check reachability *)
          let* satisfiability = check_reachability condition prio in
          begin match satisfiability with
          | `Sat -> Eval.return final_value
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
        let false_branch =
          branch (Symbolic_boolean.not cond) false prio_false
        in
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
    | Symbol sym -> Eval.return (None, sym)
    | _ ->
      let num_symbols = Thread.num_symbols thread in
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
      Eval.return (Smtml.Bitvector.to_int32 bv)
    | _ ->
      let* assign, symbol = summary_symbol i in
      let* () =
        match assign with
        | Some assign ->
          let assign = Symbolic_boolean.of_expr assign in
          add_pc assign
        | None -> Eval.return ()
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
    let path_condition = Thread.pc thread in
    let symbol_scopes = Thread.symbol_scopes thread in
    let stats = Thread.bench_stats thread in
    let* model =
      Benchmark.handle_time_span stats.solver_final_model_time @@ fun () ->
      match Solver.model_of_path_condition solver ~path_condition with
      | Some model -> Eval.return model
      | None ->
        (* It can happen when the solver is interrupted *)
        (* TODO: once https://github.com/formalsec/smtml/pull/479 is merged
             if solver was interrupted then stop else assert false *)
        stop
    in
    let labels = Thread.labels thread in
    let breadcrumbs = Thread.breadcrumbs thread in
    State_monad.return
      (Sym_eval.EError
         { kind = `Trap t; model; labels; breadcrumbs; symbol_scopes } )

  let assertion_fail c model labels breadcrumbs symbol_scopes =
    State_monad.return
      (Sym_eval.EError
         { kind = `Assertion c; model; labels; breadcrumbs; symbol_scopes } )

  let assertion (c : Symbolic_boolean.t) =
    (* TODO: better prio here *)
    let prio_false = Prio.dummy in
    let prio_true = Prio.dummy in
    let* assertion_true =
      select_inner c ~with_breadcrumbs:false ~explore_first:false ~prio_true
        ~prio_false ~check_only_true_branch:false
    in
    if assertion_true then Eval.return ()
    else
      let* thread in
      let solver = solver () in
      let path_condition = Thread.pc thread in
      let symbol_scopes = Thread.symbol_scopes thread in
      let stats = Thread.bench_stats thread in
      let* model =
        Benchmark.handle_time_span stats.solver_final_model_time @@ fun () ->
        match Solver.model_of_path_condition solver ~path_condition with
        | Some model -> Eval.return model
        | None ->
          (* It can happen when the solver is interrupted *)
          (* TODO: once https://github.com/formalsec/smtml/pull/479 is merged
               if solver was interrupted then stop else assert false *)
          stop
      in
      let breadcrumbs = Thread.breadcrumbs thread in
      let labels = Thread.labels thread in
      assertion_fail c model labels breadcrumbs symbol_scopes

  let ite (c : Symbolic_boolean.t) ~(if_true : Symbolic_value.t)
    ~(if_false : Symbolic_value.t) : Symbolic_value.t t =
    match (if_true, if_false) with
    | I32 if_true, I32 if_false ->
      Eval.return
        (Symbolic_value.I32 (Symbolic_boolean.ite c ~if_true ~if_false))
    | I64 if_true, I64 if_false ->
      Eval.return
        (Symbolic_value.I64 (Symbolic_boolean.ite c ~if_true ~if_false))
    | F32 if_true, F32 if_false ->
      Eval.return
        (Symbolic_value.F32 (Symbolic_boolean.ite c ~if_true ~if_false))
    | F64 if_true, F64 if_false ->
      Eval.return
        (Symbolic_value.F64 (Symbolic_boolean.ite c ~if_true ~if_false))
    | Ref _, Ref _ ->
      (* TODO: better prio here *)
      let prio_false = Prio.dummy in
      let prio_true = Prio.dummy in
      let+ b = select c ~prio_true ~prio_false in
      if b then if_true else if_false
    | _, _ -> assert false

  let depth () =
    let+ thread in
    Thread.depth thread

  let assume c instr_counter =
    (* TODO: better prio here *)
    let* thread in
    let depth = Thread.depth thread in
    let prio_true =
      Prio.v ~depth ~instr_counter ~distance_to_unreachable:None
    in
    let prio_false = Prio.low in
    let* assertion_true =
      select_inner c ~with_breadcrumbs:false ~explore_first:true ~prio_true
        ~prio_false ~check_only_true_branch:true
    in
    if assertion_true then Eval.return () else stop
end
