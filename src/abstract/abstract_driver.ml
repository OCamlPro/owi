(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

module Stack = Abstract_stack

let ( let* ) = Option.bind

module type DATA_STATE = sig
  type t = Abstract_state.t option * Binary.instr option

  val eval_instr : Abstract_state.t -> Binary.instr -> t
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
  end

  module JumpTarget = struct
    module Map = PatriciaTree.MakeMap (JumpKey)

    type 'a t = 'a Map.t

    let of_list = Map.of_list

    let to_list = Map.to_list

    let empty = Map.empty

    let find_opt = Map.find_opt

    let remove = Map.remove

    let append (old : 'a list t) (neww : 'a list t) =
      Map.mapi
        (fun k el ->
          match Map.find_opt k old with
          | Some el' -> List.append el el'
          | None -> el )
        neww

    let decr jt =
      remove (I 0) jt |> to_list
      |> List.map (fun (k, v) -> (JumpKey.decr k, v))
      |> of_list
  end

  let ( let> ) (opt, mapp) f =
    match opt with Some v -> f (v, mapp) | None -> (None, mapp)

  let serialize ~widens :
       Abstract_state.t
    -> Abstract_state.t
    -> (Abstract_state.t, 'a) Abstract_domain.Context.result =
   fun state_a state_b ->
    let gen_new_value ~widens a b state_a state_b
      (Abstract_domain.Context.Result (inc, intup, cont)) f =
      if Abstract_value.equal a b then None
      else
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
          let integer, out_tuple = local_cont ctx out_tuple in
          let list, out_tuple = cont ctx out_tuple in
          let b = Abstract_value.of_binary size integer in
          (f b list, out_tuple)
        in
        Some (Abstract_domain.Context.Result (inc, in_tup, cont))
    in
    let rec serialize_stack lhs rhs acc_res =
      match (lhs, rhs) with
      | [], [] -> acc_res
      | [], _ :: _ | _ :: _, [] ->
        Fmt.failwith "join on stacks of different sizes"
      | v1 :: rest_a, v2 :: rest_b -> begin
        let r = gen_new_value ~widens v1 v2 state_a state_b acc_res List.cons in
        serialize_stack rest_a rest_b
          (match r with Some res -> res | None -> acc_res)
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
          match gen_new_value ~widens v1 v2 state_a state_b res f with
          | Some res -> res
          | None -> res
        end
        state_a.locals state_b.locals
        (Abstract_domain.Context.Result
           ( true
           , Abstract_domain.Context.empty_tuple ()
           , fun _ctx out -> (state_a.locals, out) ) )
    in
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

  let widen widening_id state_a state_b =
    let (Abstract_domain.Context.Result (included, in_tuple, continue)) =
      serialize ~widens:true state_a state_b
    in
    let ctx, included, out =
      Abstract_domain.widened_fixpoint_step ~widening_id ~previous:state_a.ctx
        ~next:state_b.ctx (included, in_tuple)
    in
    let state, _ = continue ctx out in
    ({ state with ctx }, included)

  let rec eval_expr :
    t -> Binary.expr -> t option * Abstract_state.t list JumpTarget.t =
   fun state expr ->
    let rec loop (state, jt) (expr : Binary.expr) =
      match expr with
      | [] -> (Some state, jt)
      | instr :: instrs -> (
        let new_state, new_jt = eval_instr state instr.raw in
        let new_jt = JumpTarget.append jt new_jt in
        match new_state with
        | None -> (None, new_jt)
        | Some s -> loop (s, new_jt) instrs )
    in
    loop (state, JumpTarget.empty) expr

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
      List.mapi (fun i (_str_opt, vt) -> (i, init_value vt)) func.locals
      |> Abstract_locals.of_list
    in
    let fn_state =
      { state with
        stack = args
      ; ctx = state.ctx
      ; func_rt = result_type
      ; locals
      }
    in
    Log.info (fun m -> m "Func start state : %a" Abstract_state.pp fn_state);
    (* TODO: handle mapping *)
    let func_end_state, _ = eval_expr fn_state func.body.raw in
    Log.info (fun m ->
      m "Func end state : %a@."
        (Fmt.option ~none:(Fmt.any "None") Abstract_state.pp)
        func_end_state );
    (* We should probably copy state and join back the return values in the context here *)
    let* func_end_state in
    let stack =
      caller_popped_stack
      @ Stack.keep func_end_state.stack (List.length result_type)
    in
    Some { state with stack }

  and eval_instr ({ ctx; stack; env; envs; _ } as state : Abstract_state.t) :
    Binary.instr -> t option * Abstract_state.t list JumpTarget.t =
   fun instr ->
    Log.debug (fun m ->
      m "#%a\t\t%a@."
        (Binary.pp_instr ~short:true)
        instr Abstract_state.pp state );
    match instr with
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
    | Block (_str_opt, _bt, expr) ->
      (* TODO il faut gérer le mapping pour le cas de None *)
      let> { stack; locals; _ }, mapping = eval_expr state expr.raw in
      let state = { state with stack; locals } in
      let state =
        match JumpTarget.find_opt (I 0) mapping with
        | Some br_states -> List.fold_left join state br_states
        | None -> state
      in
      let mapping =
        (* TODO on peut avoir une paire de (int * map) pour ne pas avoir à decr la liste immédiatement *)
        JumpTarget.decr mapping
      in
      (Some state, mapping)
    | If_else (_, bt, expr_then, expr_else) ->
      (* TODO: use `pop_bool` once I32.to_boolean is properly implemented *)
      let b, stack = Stack.pop stack in
      let cond = Abstract_value.to_boolean ctx b in
      let state_then, jt_true =
        let> ctx, _ = (Abstract_domain.assume ctx cond, JumpTarget.empty) in
        eval_instr { state with stack; ctx } (Block (None, bt, expr_then))
      in
      let state_else, jt_false =
        let not_cond = Abstract_boolean.not ctx cond in
        let> ctx, _ = (Abstract_domain.assume ctx not_cond, JumpTarget.empty) in
        eval_instr { state with stack; ctx } (Block (None, bt, expr_else))
      in
      let jt = JumpTarget.append jt_true jt_false in
      begin match (state_then, state_else) with
      | Some state_true, Some state_false ->
        (Some (join state_true state_false), jt)
      | Some state, None | None, Some state -> (Some state, jt)
      | None, None ->
        (* TODO should this be assert false ? *)
        (None, jt)
      end
    | Loop (_str_opt, bt, body) ->
      let widening_id = Domains.Sig.Widening_Id.fresh () in
      (* TODO tester si on a besoin de copie *)
      let initial_state =
        { state with ctx = Abstract_domain.Context.copy ctx }
      in
      let rec fixpoint state =
        let next_state, jt = eval_expr state body.raw in
        let to_take =
          match bt with
          | Some (Bt_raw (_i, (params, _res))) -> List.length params
          | None -> 0
        in
        let shorten_stack stack = Stack.keep stack to_take in
        let next_head =
          match JumpTarget.find_opt (I 0) jt with
          | Some jts ->
            let fp_stack = shorten_stack initial_state.stack in
            List.fold_left
              (fun acc state ->
                let stack = shorten_stack state.Abstract_state.stack in
                join acc { state with stack } )
              { initial_state with stack = fp_stack }
              jts
          | None -> assert false
        in
        let widened, included = widen widening_id state next_head in
        if not included then fixpoint widened
        else
          (* fixpoint reached: exit loop, assume condition is false *)
          let jt = JumpTarget.decr jt in
          let next_state =
            Option.bind next_state @@ fun next_state ->
            let stack = next_state.stack @ initial_state.stack in
            Some { next_state with stack }
          in
          (next_state, jt)
      in
      fixpoint state
    | Br i -> (None, JumpTarget.of_list [ (I i, [ state ]) ])
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
    | instr -> (
      let res = S.eval_instr state instr in
      match res with
      | Some s, None -> (Some s, JumpTarget.empty)
      | None, Some instr -> eval_instr state instr
      | Some _, Some _ -> (* should not happen *) assert false
      | None, None -> (* unreachable *) (None, JumpTarget.empty) )
end

(*===========================================================================*)

module DataAbstract_state : DATA_STATE = struct
  type t = Abstract_state.t option * Binary.instr option

  let eval_i32 ({ stack; ctx; _ } as state : Abstract_state.t) :
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
      let stack = Stack.apply_i32_i32_i32 stack (Abstract_i32.div_s ctx) in
      { state with stack }
    | Div U ->
      let stack = Stack.apply_i32_i32_i32 stack (Abstract_i32.div_u ctx) in
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
    | _ ->
      Fmt.epr "not implemented yet";
      assert false

  let eval_i64 ({ stack; ctx; _ } as state : Abstract_state.t) :
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
      let stack = Stack.apply_i64_i64_i64 stack (Abstract_i64.div_s ctx) in
      { state with stack }
    | Div U ->
      let stack = Stack.apply_i64_i64_i64 stack (Abstract_i64.div_u ctx) in
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

  let eval_instr ({ stack; _ } as state : Abstract_state.t) : Binary.instr -> t
      = function
    | I32 instr ->
      let r = eval_i32 state instr in
      (Some r, None)
    | I64 instr ->
      let r = eval_i64 state instr in
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
    | ( If_else _ | Call _ | Block _ | Loop _ | Br _ | Br_if _ | Br_table _
      | Br_on_non_null _ | Br_on_null _ ) as instr ->
      (Some state, Some instr)
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

  List.iter
    begin fun (e : Binary.expr Annotated.t) ->
      let end_state, _ = ConcreteFixpoint.eval_expr state e.raw in
      Fmt.pr "End Abstract_state : %a@."
        (Fmt.option ~none:(Fmt.any "none") Abstract_state.pp)
        end_state
    end
    m.to_run
