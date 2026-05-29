(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

(* Multicore is based on several layers of monad transformers. The module as a whole is made to provide a monad to explore in parallel different possibilites, with a notion of priority. *)

(* Add a notion of faillibility to the evaluation. "Transformer without module functor" style. *)

include Symex.Monad

type 'a t = ('a, Bug.t, Prio.metrics, Thread.t) Symex.Monad.t

let prune () = fail `Prune

let add_already_checked_condition_to_pc (condition : Symbolic_boolean.t) =
  map_state (Thread.add_already_checked_condition_to_pc condition)
[@@inline]

let get_pc () =
  fold_state (fun (state : Thread.t) ->
    let pc = Symex.Path_condition.to_list state.pc in
    List.fold_left Smtml.Expr.Set.union Smtml.Expr.Set.empty pc )

let add_breadcrumb crumb = map_state (Thread.add_breadcrumb crumb)

let add_label label = map_state (Thread.add_label label)

let open_scope scope = map_state (Thread.open_scope scope)

let close_scope = map_state Thread.close_scope

let with_new_invisible_symbol ty f =
  let* sym =
    fold_state (fun (state : Thread.t) ->
      Fmt.kstr (Smtml.Symbol.make ty) "symbol_invisible_%i" state.num_symbols )
  in
  let+ () = map_state Thread.incr_num_symbols in
  f sym

let with_new_symbol ty f =
  let* sym =
    fold_state (fun (state : Thread.t) ->
      Fmt.kstr (Smtml.Symbol.make ty) "symbol_%d" state.num_symbols )
  in
  let+ () = map_state (Thread.add_symbol sym) in
  f sym

let check_reachability condition =
  fold_state (fun (state : Thread.t) ->
    let pc = Symex.Path_condition.slice_on_new_condition condition state.pc in
    Benchmark.handle_time_span state.bench_stats.solver_sat_time @@ fun () ->
    Solver.check pc condition )

let assume condition =
  let* satisfiability = check_reachability condition in
  match satisfiability with
  | `Sat ->
    let* () = add_already_checked_condition_to_pc condition in
    return ()
  | `Unsat -> prune ()
  | `Unknown -> if Solver.was_interrupted () then prune () else assert false

let select_inner ~with_breadcrumbs (condition : Symbolic_boolean.t)
  ~instr_counter_true ~instr_counter_false =
  let condition = Smtml.Typed.simplify condition in
  match Smtml.Typed.view condition with
  | Val True -> return true
  | Val False -> return false
  | _ ->
    let branch condition final_value priority =
      let* () =
        if with_breadcrumbs then add_breadcrumb (if final_value then 1 else 0)
        else return ()
      in
      let* () = yield priority in
      let* () = assume condition in
      let* () = map_state (Thread.set_priority priority) in
      return final_value
    in

    let* instr_counter, depth =
      fold_state (fun (state : Thread.t) ->
        (state.priority.instr_counter, state.depth) )
    in
    let true_branch =
      let prio =
        let instr_counter =
          Option.value instr_counter_true ~default:instr_counter
        in
        Prio.v ~instr_counter ~distance_to_unreachable:None ~depth
      in
      branch condition true prio
    in

    let false_branch =
      let prio =
        let instr_counter =
          Option.value instr_counter_false ~default:instr_counter
        in
        Prio.v ~instr_counter ~distance_to_unreachable:None ~depth
      in
      branch (Symbolic_boolean.not condition) false prio
    in

    let* () = fold_state Thread.incr_path_count in

    choose true_branch false_branch
[@@inline]

let select (cond : Symbolic_boolean.t) ~instr_counter_true ~instr_counter_false
    =
  select_inner cond ~instr_counter_true ~instr_counter_false
    ~with_breadcrumbs:true
[@@inline]

let get_model_or_prune symbol =
  let* pc, bench_stats =
    fold_state (fun (state : Thread.t) -> (state.pc, state.bench_stats))
  in
  let sat_model =
    let set = pc |> Symex.Path_condition.slice_on_symbol symbol in
    let symbol_scopes = Symbol_scope.of_symbol symbol in
    Benchmark.handle_time_span bench_stats.solver_intermediate_model_time
      (fun () -> Solver.model_of_set ~symbol_scopes ~set )
  in
  match sat_model with
  | `Unsat -> prune ()
  | `Model model ->
    begin match Smtml.Model.evaluate model symbol with
    | Some v -> return v
    | None ->
      (* the model exists so the symbol should evaluate *)
      assert false
    end
  | `Unknown -> if Solver.was_interrupted () then prune () else assert false

let select_i32 (e : Symbolic_i32.t) : int32 t =
  let e = Smtml.Typed.simplify e in
  match Smtml.Typed.view e with
  | Val (Bitv bv) when Smtml.Bitvector.numbits bv <= 32 ->
    return (Smtml.Bitvector.to_int32 bv)
  | _ ->
    let* assign, symbol =
      match Smtml.Typed.view e with
      | Symbol symbol -> return (None, symbol)
      | _ ->
        let* sym =
          fold_state (fun (state : Thread.t) ->
            Fmt.str "choice_i32_%i" state.num_symbols
            |> Smtml.Symbol.make Smtml.Typed.Types.(to_ty bitv32) )
        in
        let+ () = map_state Thread.incr_num_symbols in
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
        let* () = assume not_this_value_cond in
        generator ()
      in

      let* instr_counter, depth =
        fold_state (fun (state : Thread.t) ->
          Thread.incr_path_count state;
          (state.priority.instr_counter, state.depth) )
      in

      (* TODO: better prio here? *)
      let prio = Prio.v ~instr_counter ~distance_to_unreachable:None ~depth in

      fork ~parent:this_val_branch ~child:(prio, not_this_val_branch)
    in
    generator ()

let bug () =
  let* path_condition, solver_final_model_time =
    fold_state (fun (state : Thread.t) ->
      (state.pc, state.bench_stats.solver_final_model_time) )
  in
  let* model =
    Benchmark.handle_time_span solver_final_model_time @@ fun () ->
    match Solver.model_of_path_condition ~path_condition with
    | Some model -> return model
    | None -> if Solver.was_interrupted () then prune () else assert false
  in
  let+ state = fold_state Fun.id in
  (state, model)

let trap err =
  let* state, model = bug () in
  fail @@ `Trap { Bug.err; state; model }

let assertion (assertion : Symbolic_boolean.t) =
  (* TODO: better prio here ? *)
  let* assertion_true =
    select_inner assertion ~with_breadcrumbs:false ~instr_counter_true:None
      ~instr_counter_false:None
  in
  if assertion_true then return ()
  else
    let* state, model = bug () in
    fail @@ `Assertion { Bug.assertion; state; model }

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
