(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

module Stack = Abstract_stack

let ( let* ) = Option.bind

module type DATA_STATE = sig
  type t =
    | State of Abstract_state.t
    | Unreachable

  val eval_instr : Abstract_state.t -> Binary.instr Annotated.t -> t
end

type interpreter_state =
  { abs_state : Abstract_state.t
  ; modul : Abstract_extern.Func.t Link.Linked_module.t
  ; modules : Abstract_extern.Func.t Link.Linked_module.t Dynarray.t
  }

module DenotFixpoint (S : DATA_STATE) = struct
  module JumpKey = struct
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

  let gen_new_value ~widens a b state_a state_b
    (Abstract_domain.Context.Result (inc, intup, cont))
    (f : Abstract_value.t -> 'container -> 'container) =
    let size = Abstract_value.size_of a in
    (* inc : whether the new value is included in the old one
     * intup : symbolic repr of all variabls that will be created simultaneously
     * cont : continuation function
     *)
    let (Abstract_domain.Context.Result (inc, in_tup, local_cont)) =
      Abstract_domain.serialize_binary ~size ~widens state_a.Abstract_state.ctx
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

  let serialize ~widens :
       Abstract_state.t
    -> Abstract_state.t
    -> (Abstract_state.t, 'a) Abstract_domain.Context.result =
   fun state_a state_b ->
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

    let (Abstract_domain.Context.Result (included, in_tuple, globals_continue))
        =
      Abstract_globals.fold_on_nonequal_union
        begin fun k v1 v2 res ->
          let size =
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
          let f = Abstract_globals.add k in
          gen_new_value ~widens v1 v2 state_a state_b res f
        end
        state_a.globals state_b.globals
        (Abstract_domain.Context.Result
           (included, in_tuple, fun _ctx out -> (state_a.globals, out)) )
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
      let globals, out = globals_continue ctx out in
      let locals, out = locals_continue ctx out in
      ({ state_a with ctx; stack = List.rev stack; locals; globals }, out)
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

  let join_opt state_a state_b =
    match (state_a, state_b) with
    | Some state_a, Some state_b -> Some (join state_a state_b)
    | None, Some state | Some state, None -> Some state
    | None, None -> None

  let join_X (state_a, jt_a) (state_b, jt_b) =
    let jt = JumpTarget.append jt_a jt_b in
    match (state_a, state_b) with
    | Some state_a, Some state_b ->
      let abs_state = join state_a.abs_state state_b.abs_state in
      (Some { state_a with abs_state }, jt)
    | Some state, None | None, Some state -> (Some state, jt)
    | _, _ -> assert false

  let join_jts stack_size = function
    | None -> None
    | Some jts -> (
      match jts with
      | [] -> None
      | h :: t ->
        let abs_state =
          let h_stack = Stack.keep h.Abstract_state.stack stack_size in
          List.fold_left
            (fun acc state ->
              let stack = Stack.keep state.Abstract_state.stack stack_size in
              join acc { state with stack } )
            { h with stack = h_stack } t
        in
        Some abs_state )

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

  let exec_extern_func ({ stack; _ } : Abstract_state.t)
    (f : Abstract_extern.Func.t) =
    let open Abstract_extern.Func in
    let pop_arg (type ty) stack (arg : ty Abstract_extern.Func.telt) :
      ty * Stack.t =
      match arg with
      | I32 -> Stack.pop_i32 stack
      | I64 -> Stack.pop_i64 stack
      | F32 -> Stack.pop_f32 stack
      | F64 -> Stack.pop_f64 stack
      | V128 -> Stack.pop_v128 stack
      | Externref _ety ->
        (* TODO: handle when we start thinking about refs *)
        assert false
    in
    let rec split_args : type f r.
      Stack.t -> (f, r) Abstract_extern.Func.atype -> Stack.t * Stack.t =
     fun stack ty ->
      let[@local] split_one_arg args =
        let elt, stack = Stack.pop stack in
        let elts, stack = split_args stack args in
        (elt :: elts, stack)
      in
      match ty with
      | Mem (_, args) -> split_args stack args
      | Arg (_, args) -> split_one_arg args
      | UArg args -> split_args stack args
      | NArg (_, _, args) -> split_one_arg args
      | Res -> ([], stack)
    in
    let rec apply : type f r.
      Stack.t -> (f, r) Abstract_extern.Func.atype -> f -> r =
     fun stack ty f ->
      match ty with
      | Mem (_memid, _args) ->
        (* TODO: Handle correctly *)
        assert false
      | Arg (arg, args) ->
        let v, stack = pop_arg stack arg in
        apply stack args (f v)
      | UArg args -> apply stack args (f ())
      | NArg (_, arg, args) ->
        let v, stack = pop_arg stack arg in
        apply stack args (f v)
      | Res -> f
    in
    let (Abstract_extern.Func.Extern_func (Func (atype, rtype), func)) = f in
    let args, stack = split_args stack atype in
    let open Abstract_monad in
    let+ r = apply (List.rev args) atype func in
    let push_val (type ty) (arg : ty Abstract_extern.Func.telt) (v : ty) stack =
      match arg with
      | I32 -> Stack.push_i32 stack v
      | I64 -> Stack.push_i64 stack v
      | F32 -> Stack.push_f32 stack v
      | F64 -> Stack.push_f64 stack v
      | V128 -> Stack.push_v128 stack v
      | Externref _ty ->
        (* TODO: handle when we start thinking about refs *)
        assert false
    in
    match (rtype, r) with
    | R0, () -> stack
    | R1 t1, v1 -> push_val t1 v1 stack
    | R2 (t1, t2), (v1, v2) -> push_val t1 v1 stack |> push_val t2 v2
    | R3 (t1, t2, t3), (v1, v2, v3) ->
      push_val t1 v1 stack |> push_val t2 v2 |> push_val t3 v3
    | R4 (t1, t2, t3, t4), (v1, v2, v3, v4) ->
      push_val t1 v1 stack |> push_val t2 v2 |> push_val t3 v3 |> push_val t4 v4

  let rec eval_expr :
       interpreter_state
    -> Binary.expr Annotated.t
    -> interpreter_state option * Abstract_state.t list JumpTarget.t =
   fun state expr ->
    let rec loop state jt (expr : Binary.expr) =
      match expr with
      | [] -> (Some state, jt)
      | instr :: instrs -> (
        let new_state, new_jt = eval_instr state instr in
        let new_jt = JumpTarget.append jt new_jt in
        Log.debug (fun m ->
          m "jt after (%a) :  %a"
            (Binary.pp_instr ~short:true)
            instr.raw
            (JumpTarget.pp state.abs_state.Abstract_state.ctx)
            new_jt );
        match new_state with
        | None -> (None, new_jt)
        | Some state -> loop state new_jt instrs )
    in
    loop state JumpTarget.empty expr.raw

  and eval_func ({ abs_state; _ } as state : interpreter_state)
    (func : Binary.Func.t) =
    Log.info (fun m ->
      m "calling func  : func %s" (Option.value func.id ~default:"anonymous") );
    let (Bt_raw ((None | Some _), (param_type, result_type))) = func.type_f in
    let args, caller_popped_stack =
      Stack.pop_n abs_state.stack (List.length param_type)
    in
    let init_value : Binary.val_type -> Abstract_value.t = function
      | Num_type I32 -> I32 (Abstract_i32.zero abs_state.ctx)
      | Num_type I64 -> I64 (Abstract_i64.zero abs_state.ctx)
      | Num_type F32 -> F32 (Abstract_f32.unknown abs_state.ctx)
      | Num_type F64 -> F64 (Abstract_f64.unknown abs_state.ctx)
      | _ -> assert false
    in

    let locals =
      args @ List.map (fun (_str_opt, vt) -> init_value vt) func.locals
      |> List.rev
      |> List.mapi (fun i x -> (i, x))
      |> Abstract_locals.of_list
    in
    let fn_state =
      { abs_state with stack = []; func_rt = result_type; locals }
    in
    Log.debug (fun m ->
      m "call (%a): abstract state : %a"
        (Fmt.option ~none:(Fmt.any "$") Fmt.string)
        func.id
        (Abstract_state.pp fn_state.ctx)
        fn_state );
    (* TODO: handle mapping *)
    let func_end_state, _ =
      eval_expr { state with abs_state = fn_state } func.body
    in
    ( match func_end_state with
    | Some state ->
      Log.debug (fun m ->
        m "after call(%a): abstract state : %a@."
          (Fmt.option ~none:(Fmt.any "$") Fmt.string)
          func.id
          (Abstract_state.pp state.abs_state.ctx)
          state.abs_state )
    | None -> Log.debug (fun m -> m "abstract state : None @.") );
    (* We should probably copy state and join back the return values in the context here *)
    let* func_end_state in
    let stack =
      caller_popped_stack
      @ Stack.keep func_end_state.abs_state.stack (List.length result_type)
    in
    let abs_state =
      { abs_state with stack; ctx = func_end_state.abs_state.ctx }
    in
    Some { state with abs_state }

  and eval_instr ({ abs_state; modul; modules } as state : interpreter_state) :
       Binary.instr Annotated.t
    -> interpreter_state option * Abstract_state.t list JumpTarget.t =
   fun instr ->
    let { ctx; stack; locals; _ } : Abstract_state.t = abs_state in
    Log.debug (fun m ->
      m "abstract state : %a" (Abstract_state.pp ctx) abs_state );
    Log.info (fun m ->
      m "stack         : [ %a ]" (Abstract_stack.pp ctx) stack );
    (* Log.info (fun m -> *)
    (*   m "ctx           : [ %a ]" Abstract_domain.context_pretty ctx ); *)
    Log.info (fun m ->
      m "locals        : [ %a ]"
        (Abstract_locals.pp (Abstract_value.pp_with_ctx ctx))
        locals );
    Log.info (fun m ->
      m "running instr : %a" (Binary.pp_instr ~short:true) instr.raw );
    match instr.raw with
    | Call idx ->
      let func = Link.Linked_module.get_func modul idx in
      begin match func with
      | Wasm { func; idx } ->
        let modul = Dynarray.get modules idx in
        let r = eval_func { state with modul } func in
        (r, JumpTarget.empty)
      | Extern { idx } -> (
        let f = Link.Linked_module.get_extern_func modul idx in
        let stack = exec_extern_func abs_state f in
        match Abstract_monad.run stack abs_state with
        | None -> (None, JumpTarget.empty)
        | Some (stack, abs_state) ->
          let abs_state = { abs_state with stack } in
          (Some { state with abs_state }, JumpTarget.empty) )
      end
    | Block (_str_opt, bt, expr) ->
      let next_state, jt = eval_expr state expr in
      let stack_size =
        match bt with
        | Some (Bt_raw (_i, (params, _res))) -> List.length params
        | None -> 0
      in
      let jumps_br0 = join_jts stack_size (JumpTarget.find_opt (I 0) jt) in
      let abs_state = Option.map (fun s -> s.abs_state) next_state in
      let abs_state = join_opt abs_state jumps_br0 in
      let state =
        Option.map (fun abs_state -> { state with abs_state }) abs_state
      in
      let jt =
        (* TODO on peut avoir une paire de (int * map) pour ne pas avoir à decr la liste immédiatement *)
        JumpTarget.decr jt
      in
      (state, jt)
    | If_else (_, bt, expr_then, expr_else) ->
      let b, stack = Stack.pop_bool stack ctx in
      begin match
        ( Abstract_domain.assume ctx b
        , Abstract_domain.assume ctx (Abstract_boolean.not ctx b) )
      with
      | Some ctx, None ->
        eval_instr
          { state with abs_state = { abs_state with stack; ctx } }
          (Annotated.dummy (Binary.Block (None, bt, expr_then)))
      | None, Some ctx ->
        eval_instr
          { state with abs_state = { abs_state with stack; ctx } }
          (Annotated.dummy (Binary.Block (None, bt, expr_else)))
      | None, None -> assert false
      | Some ctx_true, Some ctx_false ->
        let strue = { abs_state with stack; ctx = ctx_true } in
        let sfalse = { abs_state with stack; ctx = ctx_false } in
        join_X
          (eval_instr
             { state with abs_state = strue }
             (Annotated.dummy (Binary.Block (None, bt, expr_then))) )
          (eval_instr
             { state with abs_state = sfalse }
             (Annotated.dummy (Binary.Block (None, bt, expr_else))) )
      end
    | Loop (_str_opt, bt, body) ->
      let widening_id = Domains.Sig.Widening_Id.fresh () in
      (* TODO tester si on a besoin de copie *)
      let initial_state =
        { abs_state with ctx = Abstract_domain.Context.copy ctx }
      in
      let stack_size =
        match bt with
        | Some (Bt_raw (_i, (params, _res))) -> List.length params
        | None -> 0
      in
      let rec fixpoint state =
        let next_state, jt = eval_expr state body in
        let next_head =
          match join_jts stack_size (JumpTarget.find_opt (I 0) jt) with
          | Some state -> Some state
          | None ->
            (* TODO: handle return too! *)
            begin match next_state with
            | Some state ->
              let stack = Stack.keep stack stack_size in
              Some { state.abs_state with stack }
            | None -> None
            end
        in

        match next_head with
        | None ->
          let jt = JumpTarget.decr jt in
          (None, jt)
        | Some next_head ->
          let widened, included = widen widening_id state.abs_state next_head in
          if not included then fixpoint { state with abs_state = widened }
          else
            (* fixpoint reached: exit loop, assume condition is false *)
            let jt = JumpTarget.decr jt in
            let next_state =
              let* next_state in
              let stack = next_state.abs_state.stack @ initial_state.stack in
              Some
                { next_state with
                  abs_state = { next_state.abs_state with stack }
                }
            in
            (next_state, jt)
      in
      fixpoint state
    | Br i -> (None, JumpTarget.of_list [ (I i, [ abs_state ]) ])
    | Br_if i ->
      let b, stack = Stack.pop_bool stack ctx in
      let jt_if =
        match Abstract_domain.assume ctx b with
        | Some ctx ->
          JumpTarget.of_list [ (I i, [ { abs_state with stack; ctx } ]) ]
        | None -> JumpTarget.empty
      in
      let state =
        match Abstract_domain.assume ctx (Abstract_boolean.not ctx b) with
        | Some ctx ->
          let abs_state = { abs_state with stack; ctx } in
          Some { state with abs_state }
        | None -> None
      in
      (state, jt_if)
    | Select _t ->
      let b, stack = Stack.pop_bool stack ctx in
      let (v1, v2), stack = Stack.pop2 stack in
      let[@inline] state_with ctx v =
        let stack = Stack.push stack v in
        let abs_state = { state.abs_state with stack; ctx } in
        Some { state with abs_state }
      in
      let state =
        match Abstract_domain.query_boolean ctx b with
        | Top ->
          (* TODO test *)
          let init_res =
            Abstract_domain.Context.Result
              ( true
              , Abstract_domain.Context.empty_tuple ()
              , fun _ctx out -> (v1, out) )
          in
          let (Abstract_domain.Context.Result (_inc, intup, cont)) =
            gen_new_value ~widens:false v1 v2 state.abs_state state.abs_state
              init_res Fun.const
          in
          let out = Abstract_domain.nondet_same_context ctx intup in
          let v = fst @@ cont ctx out in
          state_with ctx v
        | True -> state_with ctx v1
        | False -> state_with ctx v2
        | Bottom -> None
      in
      (state, JumpTarget.empty)
    | Br_table (cases, default) ->
      let v, stack = Stack.pop_i32 stack in
      let nb_cases = Array.length cases in
      let default =
        match
          Abstract_domain.assume ctx
            (Abstract_i32.ge_u ctx v (Abstract_i32.of_int ctx nb_cases))
        with
        | Some ctx -> [ (JumpKey.I default, [ { abs_state with ctx; stack } ]) ]
        | None -> []
      in
      let cases =
        Array.map
          (fun i ->
            ( i
            , Abstract_domain.assume ctx
                (Abstract_i32.eq ctx v (Abstract_i32.of_int ctx i)) ) )
          cases
      in
      let all_cases =
        Array.fold_left
          (fun acc (i, c) ->
            match c with
            | Some ctx ->
              (JumpKey.I i, [ { abs_state with ctx; stack } ]) :: acc
            | None -> acc )
          default cases
      in
      (None, JumpTarget.of_list all_cases)
    | Return -> (None, JumpTarget.of_list [ (Ret, [ abs_state ]) ])
    | _ -> (
      let res = S.eval_instr abs_state instr in
      match res with
      | State abs_state -> (Some { state with abs_state }, JumpTarget.empty)
      | Unreachable -> (None, JumpTarget.empty) )
end

module ConcreteFixpoint = DenotFixpoint (Abstract_interpreter_simple)

let eval_exprs (modul : Abstract_extern.Func.t Link.Linked_module.t) abs_state
  modules =
  let to_run = Link.Linked_module.get_expr_to_run modul in
  let state =
    List.fold_left
      (fun (state : interpreter_state) (e : Binary.expr Annotated.t) ->
        (* TODO handle this properly *)
        match ConcreteFixpoint.eval_expr state e with
        | None, _mapping -> state
        | Some state, _mapping -> state )
      { abs_state; modul; modules }
      to_run
  in
  state.abs_state

let modul_with_ctx ctx (link_state : Abstract_extern.Func.t Link.State.t)
  (modul : Abstract_extern.Func.t Link.Linked_module.t) =
  let modules = Link.State.get_modules link_state in
  let abs_state = Abstract_state.empty modul Link.Linked_module.fold_globals in
  let abs_state = { abs_state with ctx } in
  eval_exprs modul abs_state modules

let modul (link_state : Abstract_extern.Func.t Link.State.t)
  (modul : Abstract_extern.Func.t Link.Linked_module.t) =
  let modules = Link.State.get_modules link_state in
  let abs_state = Abstract_state.empty modul Link.Linked_module.fold_globals in
  eval_exprs modul abs_state modules

let exec_vfunc_from_outside ~ctx ~locals ~modul ~modules (func : Kind.func) =
  let modul = Dynarray.get modules modul in
  let abs_state =
    Abstract_state.empty_exec_state ~ctx ~locals ~modul
      Link.Linked_module.fold_globals
  in
  try
    match func with
    | Kind.Wasm { func; idx } -> (
      let modul = Dynarray.get modules idx in
      let stack =
        Abstract_locals.to_list locals
        |> List.sort (fun (i1, _) (i2, _) -> compare i1 i2)
        |> List.map snd
      in
      let abs_state = { abs_state with stack } in
      match ConcreteFixpoint.eval_func { abs_state; modul; modules } func with
      | Some state -> Ok state.abs_state
      | None -> Fmt.error_msg "failed" )
    | Extern { idx } -> (
      let f = Link.Linked_module.get_extern_func modul idx in
      let stack = ConcreteFixpoint.exec_extern_func abs_state f in
      match Abstract_monad.run stack abs_state with
      | None -> Fmt.error_msg "failed"
      | Some (stack, abs_state) ->
        let abs_state = { abs_state with stack } in
        Ok abs_state )
  with Stack_overflow -> Error `Call_stack_exhausted
