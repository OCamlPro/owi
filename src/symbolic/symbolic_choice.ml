(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

(* Multicore is based on several layers of monad transformers. The module as a whole is made to provide a monad to explore in parallel different possibilites, with a notion of priority. *)

(* Add a notion of faillibility to the evaluation. "Transformer without module functor" style. *)

include Symex.Monad

type 'a t = ('a, Bug.t, Prio.metrics, Thread.t) Symex.Monad.t

let state : Thread.t t = state ()

let solver_to_use = ref None

let solver_dls_key =
  Domain.DLS.new_key (fun () ->
    let solver_to_use = !solver_to_use in
    match solver_to_use with
    | None -> assert false
    | Some solver_to_use -> Solver.fresh solver_to_use () )

let[@inline] solver () = Domain.DLS.get solver_dls_key

let add_pc (c : Symbolic_boolean.t) =
  let c = Smtml.Typed.simplify c in
  match Smtml.Typed.view c with
  | Val True -> return ()
  | Val False -> prune ()
  | _ ->
    let* state in
    let new_thread = Thread.add_already_checked_to_pc state c in
    if Symex.Path_condition.is_unsat new_thread.pc then prune ()
    else set_state new_thread
[@@inline]

let get_pc () =
  let+ state in
  let pc = state.pc in
  let pc = Symex.Path_condition.slice pc in
  List.fold_left Smtml.Expr.Set.union Smtml.Expr.Set.empty pc

let add_breadcrumb crumb = modify_state (fun t -> Thread.add_breadcrumb t crumb)

let add_label label = modify_state (fun t -> Thread.add_label t label)

let open_scope scope = modify_state (fun t -> Thread.open_scope t scope)

let close_scope () = modify_state (fun t -> Thread.close_scope t)

let with_new_invisible_symbol ty f =
  let* state in
  let n = state.num_symbols in
  let+ () = modify_state Thread.incr_num_symbols in
  let sym = Fmt.kstr (Smtml.Symbol.make ty) "symbol_invisible_%i" n in
  f sym

let with_new_symbol ty f =
  let* state in
  let n = state.num_symbols in
  let sym = Fmt.kstr (Smtml.Symbol.make ty) "symbol_%d" n in
  let+ () =
    modify_state (fun thread ->
      let thread = Thread.add_symbol thread sym in
      Thread.incr_num_symbols thread )
  in
  f sym

let check_reachability v =
  let* state in
  if Symex.Path_condition.is_unsat state.pc then return `Unsat
  else
    let solver = solver () in
    let pc =
      Symex.Path_condition.add v state.pc
      |> Symex.Path_condition.slice_on_condition v
    in
    let stats = state.bench_stats in
    let reachability =
      Benchmark.handle_time_span stats.solver_sat_time @@ fun () ->
      Solver.check solver pc
    in
    return reachability

let get_model_or_prune symbol =
  let* state in
  if Symex.Path_condition.is_unsat state.pc then prune ()
  else
    let solver = solver () in
    let set = state.pc |> Symex.Path_condition.slice_on_symbol symbol in
    let stats = state.bench_stats in
    let symbol_scopes = Symbol_scope.of_symbol symbol in
    let sat_model =
      Benchmark.handle_time_span stats.solver_intermediate_model_time (fun () ->
        Solver.model_of_set solver ~symbol_scopes ~set )
    in
    match sat_model with
    | `Unsat -> prune ()
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
               if solver was interrupted then prune () else assert false *)
      prune ()

let select_inner ~with_breadcrumbs (cond : Symbolic_boolean.t)
  ~instr_counter_true ~instr_counter_false =
  let cond = Smtml.Typed.simplify cond in
  match Smtml.Typed.view cond with
  | Val True -> return true
  | Val False -> return false
  | _ ->
    let is_other_branch_unsat = Atomic.make false in
    let branch condition final_value priority =
      let* () =
        if with_breadcrumbs then add_breadcrumb (if final_value then 1 else 0)
        else return ()
      in
      let* () = yield priority in
      (* this is an optimisation under the assumption that the PC is always SAT (i.e. we are performing eager pruning), in such a case, when a branch is unsat, we don't have to check the reachability of the other's branch negation, because it is always going to be SAT. *)
      if Atomic.get is_other_branch_unsat then begin
        let* () = add_pc condition in
        Log.debug (fun m ->
          m "The SMT call for the %b branch was optimized away" final_value );
        (* the other branch is unsat, we must be SAT and don't need to check reachability! *)
        return final_value
      end
      else begin
        (* the other branch is SAT (or we haven't computed it yet), so we have to check reachability *)
        let* satisfiability = check_reachability condition in
        begin match satisfiability with
        | `Sat ->
          let* () = add_pc condition in
          let* () = modify_state (Thread.set_priority priority) in
          return final_value
        | `Unsat ->
          Atomic.set is_other_branch_unsat true;
          prune ()
        | `Unknown ->
          (* It can happen when the solver is interrupted *)
          (* TODO: once https://github.com/formalsec/smtml/pull/479 is merged
                                   if solver was interrupted then prune () else assert false *)
          prune ()
        end
      end
    in

    let* state in

    let prio_true =
      let instr_counter =
        match instr_counter_true with
        | None -> state.priority.instr_counter
        | Some instr_counter -> instr_counter
      in
      Prio.v ~instr_counter ~distance_to_unreachable:None ~depth:state.depth
    in
    let true_branch = branch cond true prio_true in

    let prio_false =
      let instr_counter =
        match instr_counter_false with
        | None -> state.priority.instr_counter
        | Some instr_counter -> instr_counter
      in
      Prio.v ~instr_counter ~distance_to_unreachable:None ~depth:state.depth
    in
    let false_branch = branch (Symbolic_boolean.not cond) false prio_false in
    Thread.incr_path_count state;

    choose true_branch false_branch
[@@inline]

let select (cond : Symbolic_boolean.t) ~instr_counter_true ~instr_counter_false
    =
  select_inner cond ~instr_counter_true ~instr_counter_false
    ~with_breadcrumbs:true
[@@inline]

let summary_symbol (e : Smtml.Typed.Bitv32.t) :
  (Smtml.Typed.Bool.t option * Smtml.Symbol.t) t =
  let* state in
  match Smtml.Typed.view e with
  | Symbol sym -> return (None, sym)
  | _ ->
    let num_symbols = state.num_symbols in
    let+ () = modify_state Thread.incr_num_symbols in
    let name = Fmt.str "choice_i32_%i" num_symbols in
    (* TODO: having to build two times the symbol this way is not really elegant... *)
    let sym = Smtml.Symbol.make Smtml.Typed.Types.(to_ty bitv32) name in
    let assign = Smtml.Typed.Bitv32.(eq (symbol sym) e) in
    (Some assign, sym)

let select_i32 (i : Symbolic_i32.t) : int32 t =
  match Smtml.Typed.view i with
  | Val (Bitv bv) when Smtml.Bitvector.numbits bv <= 32 ->
    return (Smtml.Bitvector.to_int32 bv)
  | _ ->
    let* assign, symbol = summary_symbol i in
    let* () =
      match assign with Some assign -> add_pc assign | None -> return ()
    in
    let rec generator () =
      let* possible_value = get_model_or_prune symbol in
      let i =
        match possible_value with
        | Smtml.Value.Bitv bv ->
          assert (Smtml.Bitvector.numbits bv <= 32);
          Smtml.Bitvector.to_int32 bv
        | _ ->
          (* it should be a value! *)
          assert false
      in
      (* TODO: everything which follows look like select_inner and could probably be simplified by calling it directly! *)
      let this_value_cond =
        Symbolic_i32.eq_concrete (Smtml.Typed.Bitv32.symbol symbol) i
      in
      let this_val_branch =
        let* () = add_breadcrumb (Int32.to_int i) in
        let* () = add_pc this_value_cond in
        return i
      in

      let not_this_value_cond = Symbolic_boolean.not this_value_cond in
      let not_this_val_branch =
        (* TODO: it should probably be something else than `add_pc` to avoid using `add_already_checked` but `add` *)
        let* () = add_pc not_this_value_cond in
        generator ()
      in
      let* state in
      Thread.incr_path_count state;

      (* TODO: better prio here? *)
      let prio =
        Prio.v ~instr_counter:state.priority.instr_counter
          ~distance_to_unreachable:None ~depth:state.depth
      in

      fork ~parent:this_val_branch ~child:(prio, not_this_val_branch)
    in
    generator ()

let bug kind =
  let* state in
  if Symex.Path_condition.is_unsat state.pc then prune ()
  else
    let* model =
      let stats = state.bench_stats in
      Benchmark.handle_time_span stats.solver_final_model_time @@ fun () ->
      let solver = solver () in
      let path_condition = state.pc in
      match Solver.model_of_path_condition solver ~path_condition with
      | Some model -> return model
      | None ->
        (* It can happen when the solver is interrupted *)
        (* TODO: once https://github.com/formalsec/smtml/pull/479 is merged
             if solver was interrupted then prune () else assert false *)
        prune ()
    in
    fail { Bug.kind; model; state }

let trap t = bug (`Trap t)

let assertion (c : Symbolic_boolean.t) =
  (* TODO: better prio here ? *)
  let* assertion_true =
    select_inner c ~with_breadcrumbs:false ~instr_counter_true:None
      ~instr_counter_false:None
  in
  if assertion_true then return () else bug (`Assertion c)

let ite (c : Symbolic_boolean.t) ~(if_true : Symbolic_value.t)
  ~(if_false : Symbolic_value.t) : Symbolic_value.t t =
  match (if_true, if_false) with
  | I32 if_true, I32 if_false ->
    let res = Symbolic_boolean.ite c if_true if_false in
    return (Symbolic_value.I32 res)
  | I64 if_true, I64 if_false ->
    let res = Symbolic_boolean.ite c if_true if_false in
    return (Symbolic_value.I64 res)
  | F32 if_true, F32 if_false ->
    return (Symbolic_value.F32 (Symbolic_boolean.ite c if_true if_false))
  | F64 if_true, F64 if_false ->
    return (Symbolic_value.F64 (Symbolic_boolean.ite c if_true if_false))
  | Ref _, Ref _ ->
    (* TODO: better prio here *)
    let+ b = select c ~instr_counter_true:None ~instr_counter_false:None in
    if b then if_true else if_false
  | _, _ -> assert false

let assume condition =
  let condition = Smtml.Typed.simplify condition in
  match Smtml.Typed.view condition with
  | Val True -> return ()
  | Val False -> prune ()
  | _ -> (
    let* satisfiability = check_reachability condition in
    match satisfiability with
    | `Sat ->
      let* () = add_pc condition in
      return ()
    | `Unsat -> prune ()
    | `Unknown ->
      (* It can happen when the solver is interrupted *)
      (* TODO: once https://github.com/formalsec/smtml/pull/479 is merged
                         if solver was interrupted then prune () else assert false *)
      prune () )
