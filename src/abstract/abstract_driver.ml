(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

module Stack = Abstract_stack

let ( let* ) = Option.bind

module type DATA_STATE = sig
  type t = Abstract_state.t option * Binary.instr Annotated.t option

  val eval_instr : Abstract_state.t -> Binary.instr Annotated.t -> t
end

(*===========================================================================*)

module DenotFixpoint (S : DATA_STATE) = struct
  type t = Abstract_state.t

  module JumpKey = struct
    (* TODO c'est mieux de définir le type nous même *)
    type t =
      | I of int
      | Ret

    let map : (int -> int) -> t -> t =
     fun f key -> match key with I i -> I (f i) | Ret -> Ret

    let decr = map Int.pred

    let to_int = function I i -> i | Ret -> -1

    let pp fmt = function I i -> Fmt.pf fmt "%i" i | Ret -> Fmt.pf fmt "ret"
  end

  module JumpTarget = struct
    include PatriciaTree.MakeMap (JumpKey)

    let append = idempotent_union (fun _ v1 v2 -> List.append v1 v2)

    let decr map =
      fold
        (fun k v acc ->
          match k with I 0 -> acc | k -> add (JumpKey.decr k) v acc )
        map empty

    let pp ctx fmt =
      let pp_v = Fmt.list ~sep:Fmt.semi (Abstract_state.pp ctx) in
      pretty (fun fmt jk v -> Fmt.pf fmt "%a -> %a" JumpKey.pp jk pp_v v) fmt
  end

  let serialize ~widens :
       Abstract_state.t
    -> Abstract_state.t
    -> (Abstract_state.t, 'a) Abstract_domain.Context.result =
   fun state_a state_b ->
    let gen_new_value ~widens a b state_a state_b
      (Abstract_domain.Context.Result (inc, intup, cont)) f =
      let size = Abstract_value.size_of a in
      (* inc : whether the new value is included in the old one
       * intup : symbolic repr of all variabls that will be created simultaneously
       * cont : continuation function
       *)
      let (Abstract_domain.Context.Result (inc, in_tup, local_cont)) =
        Abstract_domain.serialize_binary ~size ~widens
          state_a.Abstract_state.ctx
          (Abstract_value.to_binary a)
          state_b.Abstract_state.ctx
          (Abstract_value.to_binary b)
          (inc, intup)
      in
      let cont ctx out_tuple =
        let value, out_tuple = local_cont ctx out_tuple in
        let container, out_tuple = cont ctx out_tuple in
        let b = Abstract_value.of_binary size value in
        (f b container, out_tuple)
      in
      Abstract_domain.Context.Result (inc, in_tup, cont)
    in
    let rec serialize_stack lhs rhs acc_res =
      match (lhs, rhs) with
      | [], [] -> acc_res
      | [], _ :: _ | _ :: _, [] ->
        Fmt.failwith "join on stacks of different sizes"
      | v1 :: rest_a, v2 :: rest_b -> begin
        let r = gen_new_value ~widens v1 v2 state_a state_b acc_res List.cons in
        serialize_stack rest_a rest_b r
        end
    in
    let (Abstract_domain.Context.Result (included, in_tuple, locals_continue)) =
      Abstract_locals.fold_on_nonequal_union
        begin fun k v1 v2 res ->
          let size =
            (* v1 and v2 should have the same size *)
            match v1 with
            | Some v -> Abstract_value.size_of v
            | None -> assert false
          in
          let v1 =
            Option.value v1 ~default:(Abstract_value.top size state_a.ctx)
          in
          let v2 =
            Option.value v2 ~default:(Abstract_value.top size state_b.ctx)
          in
          let f = Abstract_locals.add k in
          gen_new_value ~widens v1 v2 state_a state_b res f
        end
        state_a.locals state_b.locals
        (Abstract_domain.Context.Result
           ( true
           , Abstract_domain.Context.empty_tuple ()
           , fun _ctx out -> (state_a.locals, out) ) )
    in

    Log.debug (fun m ->
      let pp_locals ctx = Abstract_locals.pp (Abstract_value.pp_with_ctx ctx) in
      m "serializing locals (%s) : @\n first : %a @\n second : %a"
        (if widens then "widen" else "join")
        (pp_locals state_a.ctx) state_a.locals (pp_locals state_b.ctx)
        state_b.locals );

    Log.debug (fun m ->
      m "serializing stacks (%s) : @\n first : %a @\n second : %a"
        (if widens then "widen" else "join")
        (Abstract_stack.pp state_a.ctx)
        state_a.stack
        (Abstract_stack.pp state_b.ctx)
        state_b.stack );

    let (Abstract_domain.Context.Result (inc, in_tup, stack_continue)) =
      serialize_stack state_a.stack state_b.stack
        (Abstract_domain.Context.Result
           (included, in_tuple, fun _ctx out -> ([], out)) )
    in
    let cont ctx out =
      let stack, out = stack_continue ctx out in
      let locals, out = locals_continue ctx out in
      ({ state_a with ctx; stack = List.rev stack; locals }, out)
    in
    Abstract_domain.Context.Result (inc, in_tup, cont)

  let join state_a state_b =
    let (Abstract_domain.Context.Result (_inc, in_tuple, continue)) =
      serialize ~widens:false state_a state_b
    in
    let ctx, out =
      Abstract_domain.typed_nondet2 state_a.ctx state_b.ctx in_tuple
    in
    fst @@ continue ctx out

  let join_X (state_a, jt_a) (state_b, jt_b) =
    let jt = JumpTarget.append jt_a jt_b in
    match (state_a, state_b) with
    | Some state_a, Some state_b -> (Some (join state_a state_b), jt)
    | Some state, None | None, Some state -> (Some state, jt)
    | _, _ -> assert false

  let widen widening_id state_a state_b =
    let (Abstract_domain.Context.Result (included, in_tuple, continue)) =
      serialize ~widens:true state_a state_b
    in
    let ctx, included, out =
      Abstract_domain.widened_fixpoint_step ~widening_id ~previous:state_a.ctx
        ~next:state_b.ctx (included, in_tuple)
    in
    (* TODO find out why is the out tuple ignored *)
    let state, _out_tuple = continue ctx out in
    ({ state with ctx }, included)

  let rec eval_expr :
       t
    -> Binary.expr Annotated.t
    -> t option * Abstract_state.t list JumpTarget.t =
   fun state expr ->
    let rec loop (state, jt) (expr : Binary.expr) =
      match expr with
      | [] -> (Some state, jt)
      | instr :: instrs -> (
        let new_state, new_jt = eval_instr state instr in
        let new_jt = JumpTarget.append jt new_jt in
        Log.debug (fun m ->
          m "jt            :  %a"
            (JumpTarget.pp state.Abstract_state.ctx)
            new_jt );
        match new_state with
        | None -> (None, new_jt)
        | Some s -> loop (s, new_jt) instrs )
    in
    loop (state, JumpTarget.empty) expr.raw

  and eval_func (state : Abstract_state.t) (func : Binary.Func.t) =
    Log.info (fun m ->
      m "calling func  : func %s" (Option.value func.id ~default:"anonymous") );
    let (Bt_raw ((None | Some _), (param_type, result_type))) = func.type_f in
    let args, caller_popped_stack =
      Stack.pop_n state.stack (List.length param_type)
    in
    let init_value : Binary.val_type -> Abstract_value.t = function
      | Num_type I32 -> I32 (Abstract_i32.zero state.ctx)
      | Num_type I64 -> I64 (Abstract_i64.zero state.ctx)
      | _ -> assert false
    in

    let locals =
      (* TODO check args position *)
      args @ List.map (fun (_str_opt, vt) -> init_value vt) func.locals
      |> List.mapi (fun i x -> (i, x))
      |> Abstract_locals.of_list
    in
    let fn_state = { state with stack = []; func_rt = result_type; locals } in
    Log.debug (fun m ->
      m "call (%a): abstract state : %a"
        (Fmt.option ~none:(Fmt.any "$") Fmt.string)
        func.id
        (Abstract_state.pp fn_state.ctx)
        fn_state );
    (* TODO: handle mapping *)
    let func_end_state, _ = eval_expr fn_state func.body in
    ( match func_end_state with
    | Some state ->
      Log.debug (fun m ->
        m "after call(%a): abstract state : %a@."
          (Fmt.option ~none:(Fmt.any "$") Fmt.string)
          func.id
          (Abstract_state.pp state.ctx)
          state )
    | None -> Log.debug (fun m -> m "abstract state : None @.") );
    (* We should probably copy state and join back the return values in the context here *)
    let* func_end_state in
    let stack =
      caller_popped_stack
      @ Stack.keep func_end_state.stack (List.length result_type)
    in
    Some { state with stack; ctx = func_end_state.ctx }

  and eval_instr
    ({ ctx; stack; env; envs; locals; _ } as state : Abstract_state.t) :
    Binary.instr Annotated.t -> t option * Abstract_state.t list JumpTarget.t =
   fun instr ->
    Log.debug (fun m ->
      m "abstract state : %a" (Abstract_state.pp state.ctx) state );
    Log.info (fun m -> m "stack         : [ %a ]" (Abstract_stack.pp ctx) stack);
    (* Log.info (fun m -> *)
    (*   m "ctx           : [ %a ]" Abstract_domain.context_pretty ctx ); *)
    Log.info (fun m ->
      m "locals        : [ %a ]"
        (Abstract_locals.pp (Abstract_value.pp_with_ctx state.ctx))
        locals );
    Log.info (fun m ->
      m "running instr : %a" (Binary.pp_instr ~short:true) instr.raw );
    match instr.raw with
    | Call idx ->
      let func = Link_env.get_func env idx in
      begin match func with
      | Wasm { func; idx } ->
        let env = Dynarray.get envs idx in
        let r = eval_func { state with env } func in
        (r, JumpTarget.empty)
      | Extern { idx } -> (
        match idx with
        | 0 ->
          let v = Abstract_i32.unknown ctx in
          let stack = Stack.push_i32 stack v in
          (Some { state with stack }, JumpTarget.empty)
        | 1 ->
          let v = Abstract_i64.unknown ctx in
          let stack = Stack.push_i64 stack v in
          (Some { state with stack }, JumpTarget.empty)
        | _ ->
          Fmt.failwith "Some day we will have proper external function support"
        )
      end
    | Block (_str_opt, _bt, expr) -> (
      match eval_expr state expr with
      | None, jt -> (None, JumpTarget.decr jt)
      | Some state, jt ->
        let state =
          match JumpTarget.find_opt (I 0) jt with
          | Some br_states -> List.fold_left join state br_states
          | None -> state
        in
        let state =
          match JumpTarget.find_opt Ret jt with
          | Some ret_states -> List.fold_left join state ret_states
          | None -> state
        in
        let jt =
          (* TODO on peut avoir une paire de (int * map) pour ne pas avoir à decr la liste immédiatement *)
          JumpTarget.decr jt
        in
        (Some state, jt) )
    | If_else (_, bt, expr_then, expr_else) ->
      let b, stack = Stack.pop_i32 stack in
      begin match
        ( Abstract_domain.assume ctx
            (Abstract_i32.ne ctx (Abstract_i32.of_int ctx 0) b)
        , Abstract_domain.assume ctx (Abstract_i32.eqz ctx b) )
      with
      | Some ctx, None ->
        eval_instr { state with stack; ctx }
          (Annotated.dummy (Binary.Block (None, bt, expr_then)))
      | None, Some ctx ->
        eval_instr { state with stack; ctx }
          (Annotated.dummy (Binary.Block (None, bt, expr_else)))
      | None, None -> assert false
      | Some ctx_true, Some ctx_false ->
        join_X
          (eval_instr
             { state with stack; ctx = ctx_true }
             (Annotated.dummy (Binary.Block (None, bt, expr_then))) )
          (eval_instr
             { state with stack; ctx = ctx_false }
             (Annotated.dummy (Binary.Block (None, bt, expr_else))) )
      end
    | Loop (_str_opt, bt, body) ->
      let widening_id = Domains.Sig.Widening_Id.fresh () in
      (* TODO tester si on a besoin de copie *)
      let initial_state =
        { state with ctx = Abstract_domain.Context.copy ctx }
      in
      let to_take =
        match bt with
        | Some (Bt_raw (_i, (params, _res))) -> List.length params
        | None -> 0
      in
      let rec fixpoint state =
        let next_state, jt = eval_expr state body in
        let shorten_stack stack = Stack.keep stack to_take in
        let next_head =
          let handle_jts jts =
            let fp_stack = shorten_stack initial_state.stack in
            List.fold_left
              (fun acc state ->
                let stack = shorten_stack state.Abstract_state.stack in
                let joined = join acc { state with stack } in
                joined )
              { initial_state with stack = fp_stack }
              jts
          in
          match JumpTarget.find_opt (I 0) jt with
          | Some jts -> handle_jts jts
          | None -> (
            (* TODO handle rets *)
            match next_state with
            | Some state ->
              let stack = shorten_stack state.stack in
              { state with stack }
            | None ->
              (* Should not be possible
                     We have no targets to jump to and no state to continue on during the fixpoint iteration
                   *)
              assert false )
        in
        let widened, included = widen widening_id state next_head in
        if not included then fixpoint widened
        else
          (* fixpoint reached: exit loop, assume condition is false *)
          let jt = JumpTarget.decr jt in
          let next_state =
            let* next_state in
            let stack = next_state.stack @ initial_state.stack in
            Some { next_state with stack }
          in
          (next_state, jt)
      in
      fixpoint state
    | Br i -> (None, JumpTarget.of_list [ (I i, [ state ]) ])
    | Br_if i ->
      let b, stack = Stack.pop_i32 stack in
      let jt_if =
        match
          Abstract_domain.assume ctx
            (Abstract_i32.ne ctx (Abstract_i32.of_int ctx 0) b)
        with
        | Some ctx ->
          JumpTarget.of_list [ (I i, [ { state with stack; ctx } ]) ]
        | None -> JumpTarget.empty
      in
      let new_state =
        match Abstract_domain.assume ctx (Abstract_i32.eqz ctx b) with
        | Some ctx -> Some { state with stack; ctx }
        | None -> None
      in
      (new_state, jt_if)
    | Br_table (cases, default) ->
      let v, stack = Stack.pop_i32 stack in
      let equals =
        let f acc i =
          let predicate = Abstract_i32.of_int ctx i |> Abstract_i32.eq ctx v in
          match Abstract_domain.assume ctx predicate with
          | Some _ -> i :: acc
          | None -> acc
        in
        Array.fold_left f [] cases
      in
      let non_equals =
        let f acc i =
          Abstract_i32.of_int ctx i |> Abstract_i32.eq ctx v
          |> Abstract_boolean.not ctx
          |> Abstract_boolean.and_ ctx acc
        in
        let predicate = Array.fold_left f (Abstract_boolean.true_ ctx) cases in
        match Abstract_domain.assume ctx predicate with
        | Some _ -> [ default ]
        | None -> []
      in
      let jt_list =
        List.append equals non_equals
        |> List.map (fun i -> (JumpKey.I i, [ { state with stack } ]))
      in
      (None, JumpTarget.of_list jt_list)
    | Return -> (None, JumpTarget.of_list [ (Ret, [ state ]) ])
    | _ -> (
      let res = S.eval_instr state instr in
      match res with
      | Some s, None -> (Some s, JumpTarget.empty)
      | None, Some instr -> eval_instr state instr
      | Some _, Some _ -> (* should not happen *) assert false
      | None, None -> (* unreachable *) (None, JumpTarget.empty) )
end

(*===========================================================================*)

module DataAbstract_state : DATA_STATE = struct
  type t = Abstract_state.t option * Binary.instr Annotated.t option

  let i32_can_be_zero ctx v =
    Option.is_some @@ Abstract_domain.assume ctx (Abstract_i32.eqz ctx v)

  let eval_i32 ({ stack; ctx; invariant; _ } as state : Abstract_state.t) uuid :
    Binary.i32_instr -> _ = function
    | Const i ->
      let stack = Stack.push_i32 stack (Abstract_i32.of_int32 ctx i) in
      { state with stack }
    | Add ->
      let stack = Stack.apply_i32_i32_i32 stack (Abstract_i32.add ctx) in
      { state with stack }
    | Sub ->
      let stack = Stack.apply_i32_i32_i32 stack (Abstract_i32.sub ctx) in
      { state with stack }
    | Mul ->
      let stack = Stack.apply_i32_i32_i32 stack (Abstract_i32.mul ctx) in
      { state with stack }
    | Div S ->
      let (hd1, hd2), stack = Stack.pop2_i32 stack in
      let () =
        let possible = i32_can_be_zero ctx hd2 in
        Abstract_invariant.add_divide_by_zero_invariant invariant ~uuid
          ~possible
      in
      let stack = Stack.push_i32 stack (Abstract_i32.div_s ctx hd1 hd2) in
      { state with stack }
    | Div U ->
      let (hd1, hd2), stack = Stack.pop2_i32 stack in
      let () =
        let possible = i32_can_be_zero ctx hd2 in
        Abstract_invariant.add_divide_by_zero_invariant invariant ~uuid
          ~possible
      in
      let stack = Stack.push_i32 stack (Abstract_i32.div_u ctx hd1 hd2) in
      { state with stack }
    | Rem S ->
      let (hd1, hd2), stack = Stack.pop2_i32 stack in
      let () =
        let possible = i32_can_be_zero ctx hd2 in
        Abstract_invariant.add_divide_by_zero_invariant invariant ~uuid
          ~possible
      in
      let stack = Stack.push_i32 stack (Abstract_i32.rem_s ctx hd1 hd2) in
      { state with stack }
    | Rem U ->
      let (hd1, hd2), stack = Stack.pop2_i32 stack in
      let () =
        let possible = i32_can_be_zero ctx hd2 in
        Abstract_invariant.add_divide_by_zero_invariant invariant ~uuid
          ~possible
      in
      let stack = Stack.push_i32 stack (Abstract_i32.rem_u ctx hd1 hd2) in
      { state with stack }
    | And ->
      let stack = Stack.apply_i32_i32_i32 stack (Abstract_i32.and_ ctx) in
      { state with stack }
    | Or ->
      let stack = Stack.apply_i32_i32_i32 stack (Abstract_i32.or_ ctx) in
      { state with stack }
    | Lt S ->
      let stack =
        Stack.apply_i32_i32_boolean stack ctx (Abstract_i32.lt_s ctx)
      in
      { state with stack }
    | Lt U ->
      let stack =
        Stack.apply_i32_i32_boolean stack ctx (Abstract_i32.lt_u ctx)
      in
      { state with stack }
    | Le S ->
      let stack =
        Stack.apply_i32_i32_boolean stack ctx (Abstract_i32.le_s ctx)
      in
      { state with stack }
    | Le U ->
      let stack =
        Stack.apply_i32_i32_boolean stack ctx (Abstract_i32.le_u ctx)
      in
      { state with stack }
    | Ne ->
      let stack = Stack.apply_i32_i32_boolean stack ctx (Abstract_i32.ne ctx) in
      { state with stack }
    | Eqz ->
      let stack = Stack.apply_i32_boolean stack ctx (Abstract_i32.eqz ctx) in
      { state with stack }
    | Eq ->
      let stack = Stack.apply_i32_i32_boolean stack ctx (Abstract_i32.eq ctx) in
      { state with stack }
    | instr ->
      Fmt.epr "not implemented yet: %a" Binary.pp_i32_instr instr;
      assert false

  let i64_can_be_zero ctx v =
    Option.is_some @@ Abstract_domain.assume ctx (Abstract_i64.eqz ctx v)

  let eval_i64 ({ stack; ctx; invariant; _ } as state : Abstract_state.t) uuid :
    Binary.i64_instr -> _ = function
    | Const i ->
      let stack = Stack.push_i64 stack (Abstract_i64.of_int64 ctx i) in
      { state with stack }
    | Add ->
      let stack = Stack.apply_i64_i64_i64 stack (Abstract_i64.add ctx) in
      { state with stack }
    | Sub ->
      let stack = Stack.apply_i64_i64_i64 stack (Abstract_i64.sub ctx) in
      { state with stack }
    | Mul ->
      let stack = Stack.apply_i64_i64_i64 stack (Abstract_i64.mul ctx) in
      { state with stack }
    | Div S ->
      let (hd1, hd2), stack = Stack.pop2_i64 stack in
      let () =
        let possible = i64_can_be_zero ctx hd2 in
        Abstract_invariant.add_divide_by_zero_invariant invariant ~uuid
          ~possible
      in
      let stack = Stack.push_i64 stack (Abstract_i64.div_s ctx hd1 hd2) in
      { state with stack }
    | Div U ->
      let (hd1, hd2), stack = Stack.pop2_i64 stack in
      let () =
        let possible = i64_can_be_zero ctx hd2 in
        Abstract_invariant.add_divide_by_zero_invariant invariant ~uuid
          ~possible
      in
      let stack = Stack.push_i64 stack (Abstract_i64.div_u ctx hd1 hd2) in
      { state with stack }
    | Rem S ->
      let (hd1, hd2), stack = Stack.pop2_i64 stack in
      let () =
        let possible = i64_can_be_zero ctx hd2 in
        Abstract_invariant.add_divide_by_zero_invariant invariant ~uuid
          ~possible
      in
      let stack = Stack.push_i64 stack (Abstract_i64.rem_s ctx hd1 hd2) in
      { state with stack }
    | Rem U ->
      let (hd1, hd2), stack = Stack.pop2_i64 stack in
      let () =
        let possible = i64_can_be_zero ctx hd2 in
        Abstract_invariant.add_divide_by_zero_invariant invariant ~uuid
          ~possible
      in
      let stack = Stack.push_i64 stack (Abstract_i64.rem_u ctx hd1 hd2) in
      { state with stack }
    | And ->
      let stack = Stack.apply_i64_i64_i64 stack (Abstract_i64.and_ ctx) in
      { state with stack }
    | Or ->
      let stack = Stack.apply_i64_i64_i64 stack (Abstract_i64.or_ ctx) in
      { state with stack }
    | Lt S ->
      let stack =
        Stack.apply_i64_i64_boolean stack ctx (Abstract_i64.lt_s ctx)
      in
      { state with stack }
    | Lt U ->
      let stack =
        Stack.apply_i64_i64_boolean stack ctx (Abstract_i64.lt_u ctx)
      in
      { state with stack }
    | Le S ->
      let stack =
        Stack.apply_i64_i64_boolean stack ctx (Abstract_i64.le_s ctx)
      in
      { state with stack }
    | Le U ->
      let stack =
        Stack.apply_i64_i64_boolean stack ctx (Abstract_i64.le_u ctx)
      in
      { state with stack }
    | _ -> assert false

  let eval_local ({ stack; locals; _ } as state : Abstract_state.t) :
    Binary.local_instr -> _ = function
    | Get i ->
      let v = Abstract_locals.find i locals in
      let stack = Stack.push stack v in
      { state with stack }
    | Set i ->
      let e, stack = Stack.pop stack in
      let locals = Abstract_locals.add i e locals in
      { state with stack; locals }
    | Tee i ->
      let e, stack = Stack.pop stack in
      let stack = Stack.push stack e in
      let locals = Abstract_locals.add i e locals in
      { state with stack; locals }

  let eval_instr ({ stack; _ } as state : Abstract_state.t) :
    Binary.instr Annotated.t -> t =
   fun ({ raw; uuid; _ } as instr) ->
    match raw with
    | I32 instr ->
      let r = eval_i32 state uuid instr in
      (Some r, None)
    | I64 instr ->
      let r = eval_i64 state uuid instr in
      (Some r, None)
    | Unreachable ->
      (*TODO à gèrer proprement*)
      (None, None)
    | Local instr ->
      let state = eval_local state instr in
      (Some state, None)
    | Drop ->
      let _, stack = Stack.pop stack in
      (Some { state with stack }, None)
    | Nop -> (Some state, None)
    | If_else _ | Call _ | Block _ | Loop _ | Br _ | Br_if _ | Br_table _
    | Br_on_non_null _ | Br_on_null _ ->
      (None, Some instr)
    | instr ->
      Fmt.failwith "DataAbstract_state.eval_instr not implemented for %a"
        (Binary.pp_instr ~short:true)
        instr
end

module ConcreteFixpoint = DenotFixpoint (DataAbstract_state)

let expr (link_state : Abstract_extern_func.extern_func Link.State.t)
  (m : Abstract_extern_func.extern_func Linked.Module.t) =
  let envs = Link.State.get_envs link_state in
  let state = Abstract_state.empty m.env envs () in

  let state =
    List.fold_left
      (fun (state : Abstract_state.t) (e : Binary.expr Annotated.t) ->
        (* TODO handle this properly *)
        match ConcreteFixpoint.eval_expr state e with
        | None, _mapping -> state
        | Some state, _mapping -> state )
      state m.to_run
  in

  state.invariant
