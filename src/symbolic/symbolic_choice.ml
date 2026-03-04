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

let add_already_checked_condition_to_pc (condition : Symbolic_boolean.t) =
  let* state in
  let state = Thread.add_already_checked_condition_to_pc state condition in
  set_state state
[@@inline]

let get_pc () =
  let+ state in
  let pc = Symex.Path_condition.to_list state.pc in
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
    modify_state (fun state ->
      let state = Thread.add_symbol state sym in
      Thread.incr_num_symbols state )
  in
  f sym

let check_reachability condition =
  let* state in
  let solver = solver () in
  let pc = Symex.Path_condition.slice_on_new_condition condition state.pc in
  (* the condition must be simplified only *after* slicing when we don't know if it is SAT, otherwise, we may exclude some slices and go to an unsat path! *)
  let condition =
    let equalities = Symex.Path_condition.get_known_equalities state.pc in
    Smtml.Expr.inline_symbol_values equalities
      (Smtml.Typed.Unsafe.unwrap condition)
    |> Smtml.Typed.Unsafe.wrap
  in
  let stats = state.bench_stats in
  let reachability =
    Benchmark.handle_time_span stats.solver_sat_time @@ fun () ->
    Solver.check solver pc condition
  in
  return reachability

let get_model_or_prune symbol =
  let* state in
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
  let branch condition final_value priority =
    let* () =
      if with_breadcrumbs then add_breadcrumb (if final_value then 1 else 0)
      else return ()
    in
    let* () = yield priority in
    let* satisfiability = check_reachability condition in
    begin match satisfiability with
    | `Sat ->
      let* () = add_already_checked_condition_to_pc condition in
      let* () = modify_state (Thread.set_priority priority) in
      return final_value
    | `Unsat -> prune ()
    | `Unknown ->
      (* It can happen when the solver is interrupted *)
      (* TODO: once https://github.com/formalsec/smtml/pull/479 is merged
                                     if solver was interrupted then prune () else assert false *)
      prune ()
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

let select_i32 (e : Symbolic_i32.t) : int32 t =
  let* state in
  let equalities = Symex.Path_condition.get_known_equalities state.pc in
  let e =
    Smtml.Expr.inline_symbol_values equalities (Smtml.Typed.Unsafe.unwrap e)
    |> Smtml.Typed.Unsafe.wrap |> Smtml.Typed.simplify
  in
  match Smtml.Typed.view e with
  | Val (Bitv bv) when Smtml.Bitvector.numbits bv <= 32 ->
    return (Smtml.Bitvector.to_int32 bv)
  | _ ->
    let* assign, symbol =
      match Smtml.Typed.view e with
      | Symbol symbol -> return (None, symbol)
      | _ ->
        let num_symbols = state.num_symbols in
        let+ () = modify_state Thread.incr_num_symbols in
        let name = Fmt.str "choice_i32_%i" num_symbols in
        let sym = Smtml.Symbol.make Smtml.Typed.Types.(to_ty bitv32) name in
        let assign = Smtml.Typed.Bitv32.(eq (symbol sym) e) in
        (Some assign, sym)
    in
    let* () =
      match assign with
      | Some assign ->
        (* it must be SAT because we only introduced a constraint: new_symbol = e *)
        add_already_checked_condition_to_pc assign
      | None -> return ()
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
        let* () = add_already_checked_condition_to_pc this_value_cond in
        return i
      in

      let not_this_val_branch =
        let not_this_value_cond = Symbolic_boolean.not this_value_cond in
        (* TODO: this is annoying as the next call to get_model_or_prune is also going to check for satisfiability which is useless! it can probably be simplified.
           I'm leaving it for now to be sure this is correct. It may actually not be required but better safe than sorry! *)
        let* satisfiability = check_reachability not_this_value_cond in
        match satisfiability with
        | `Sat ->
          let* () = add_already_checked_condition_to_pc not_this_value_cond in
          generator ()
        | `Unsat -> prune ()
        | `Unknown ->
          (* It can happen when the solver is interrupted *)
          (* TODO: once https://github.com/formalsec/smtml/pull/479 is merged
                           if solver was interrupted then prune () else assert false *)
          prune ()
      in
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
  let* satisfiability = check_reachability condition in
  match satisfiability with
  | `Sat ->
    let* () = add_already_checked_condition_to_pc condition in
    return ()
  | `Unsat -> prune ()
  | `Unknown ->
    (* It can happen when the solver is interrupted *)
    (* TODO: once https://github.com/formalsec/smtml/pull/479 is merged
                           if solver was interrupted then prune () else assert false *)
    prune ()
