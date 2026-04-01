open Binary
open Abs_datastructures
module ADomain = Abs_value.ADomain
module Size = Abs_value.Size
module IntMap = Map.Make (Int)

type value = Abs_value.t

type state =
  { ctx : ADomain.Context.t
  ; stack : value Stack.t
  ; locals : value IntMap.t
  ; func_rt : val_type list
  ; env : Abs_extern_func.extern_func Link_env.t
  ; envs : Abs_extern_func.extern_func Link_env.t Dynarray.t
  }

module Flags = Operator.Flags

let pp_state : Format.formatter -> state -> unit =
 fun fmt state ->
  Fmt.pf fmt "{@\n@[<hov 2>  ctx : %a,@;stack : %a,@;locals : %a@]@\n}"
    ADomain.context_pretty state.ctx
    (Stack.pp @@ Abs_value.pp state.ctx)
    state.stack
    (Fmt.list ~sep:Fmt.semi (Abs_value.pp state.ctx))
    (IntMap.to_list state.locals |> List.map snd)

module type DATA_STATE = sig
  type t =
    | Ret of state
    | Control_flow of state * instr

  val eval_instr : state -> instr -> t
end

(*===========================================================================*)

module DenotFixpoint (S : DATA_STATE) = struct
  type t = state

  let serialize ~widens : state -> state -> (state, 'a) ADomain.Context.result =
   fun state_a state_b ->
    let rec serialize_lists lhs rhs acc_res =
      match (lhs, rhs) with
      | [], [] -> acc_res
      | [], _ :: _ | _ :: _, [] ->
        Fmt.failwith "join on stacks of different sizes"
      | v1 :: rest_a, v2 :: rest_b -> begin
        let (ADomain.Context.Result (included, in_tuple, continue)) = acc_res in
        if Abs_value.equal v1 v2 then serialize_lists rest_a rest_b acc_res
        else
          let size = Abs_value.size_of v1 in
          let (ADomain.Context.Result (included, in_tuple, local_continue)) =
            ADomain.serialize_binary ~size ~widens state_a.ctx
              (Abs_value.to_binary v1) state_b.ctx (Abs_value.to_binary v2)
              (included, in_tuple)
          in
          let new_result =
            ADomain.Context.Result
              ( included
              , in_tuple
              , fun ctx out_tuple ->
                  let integer, out_tuple = local_continue ctx out_tuple in
                  let stack, out_tuple = continue ctx out_tuple in
                  (Abs_value.of_binary size integer :: stack, out_tuple) )
          in
          serialize_lists rest_a rest_b new_result
      end
    in
    let locals_result =
      serialize_lists
        ( IntMap.to_list state_a.locals
        |> List.sort (fun (k1, _) (k2, _) -> Int.compare k1 k2)
        |> List.map snd )
        ( IntMap.to_list state_b.locals
        |> List.sort (fun (k1, _) (k2, _) -> Int.compare k1 k2)
        |> List.map snd )
        (ADomain.Context.Result
           (true, ADomain.Context.empty_tuple (), fun _ctx out -> ([], out)) )
    in
    let (ADomain.Context.Result (included, in_tuple, locals_continue)) =
      locals_result
    in

    let stack_result =
      serialize_lists
        (Stack.to_list state_a.stack)
        (Stack.to_list state_b.stack)
        (ADomain.Context.Result (included, in_tuple, fun _ctx out -> ([], out)))
    in

    let (ADomain.Context.Result (included, in_tuple, stack_continue)) =
      stack_result
    in
    ADomain.Context.Result
      ( included
      , in_tuple
      , fun ctx out ->
          let stack, out = stack_continue ctx out in
          let locals, out = locals_continue ctx out in
          ( { state_a with
              ctx
            ; stack = Stack.of_list @@ List.rev stack
            ; locals =
                IntMap.of_list
                @@ List.mapi (fun i v -> (i, v))
                @@ List.rev locals
            }
          , out ) )

  let join state_a state_b =
    let (ADomain.Context.Result (_inc, in_tuple, continue)) =
      serialize ~widens:false state_a state_b
    in
    let ctx, out = ADomain.typed_nondet2 state_a.ctx state_b.ctx in_tuple in
    fst @@ continue ctx out

  let rec eval_expr : t -> instr Annotated.t list -> t =
   fun state expr ->
    List.fold_left
      (fun acc (x : instr Annotated.t) -> eval_instr acc x.raw)
      state expr

  and eval_func (state : state) (func : Func.t) =
    Log.info (fun m ->
      m "calling func  : func %s" (Option.value func.id ~default:"anonymous") );
    let (Bt_raw ((None | Some _), (param_type, result_type))) = func.type_f in
    let args, caller_popped_stack =
      Stack.pop_n state.stack (List.length param_type)
    in
    let init_value v =
      let size = Abs_value.size_of v in
      let zero = ADomain.Binary_Forward.biconst ~size Z.zero state.ctx in
      match v with Abs_value.I32 _ -> Abs_value.I32 zero | I64 _ -> I64 zero
    in
    let fn_state =
      { state with
        stack = Stack.empty
      ; ctx = state.ctx
      ; func_rt = result_type
      ; locals =
          IntMap.of_list
          @@ List.mapi (fun i v -> (i, init_value v))
          @@ Stack.to_list args
      }
    in
    Log.info (fun m -> m "Func start state : %a" pp_state fn_state);
    let func_end_state = eval_expr fn_state func.body.raw in
    (* We should probably copy state and join back the return values in the context here *)
    Log.info (fun m -> m "Func end state : %a@." pp_state func_end_state);
    { state with
      stack =
        Stack.append caller_popped_stack
        @@ Stack.take (List.length result_type) func_end_state.stack
    }

  and eval_instr : t -> instr -> t =
   fun state instr ->
    Log.debug (fun m ->
      m "#%a\t\t%a@." (pp_instr ~short:true) instr pp_state state );
    match instr with
    | Call idx ->
      let func = Link_env.get_func state.env idx in
      begin match func with
      | Wasm { func; idx } ->
        let env = Dynarray.get state.envs idx in
        eval_func { state with env } func
      | Extern { idx } -> (
        match idx with
        | 0 ->
          (* assume that it's correctly typed to returning an i32 *)
          let v = ADomain.binary_unknown ~size:Size.b32 state.ctx in
          let stack = Stack.push state.stack (I32 v) in
          { state with stack }
        | 1 ->
          (* assume that it's correctly typed to returning an i64 *)
          let v = ADomain.binary_unknown ~size:Size.b64 state.ctx in
          let stack = Stack.push state.stack (I64 v) in
          { state with stack }
        | _ ->
          Fmt.failwith "Some day we will have proper external function support"
        )
      end
    | Block (_, bt, expr) -> (
      match bt with
      | Some (Bt_raw (_, ft)) ->
        let param_type, result_type = ft in
        let block_stack = Stack.take (List.length param_type) state.stack in
        let res = eval_expr { state with stack = block_stack } expr.raw in
        let stack = Stack.take (List.length result_type) res.stack in
        { state with
          stack = Stack.append state.stack stack
        ; locals = res.locals
        }
      | None -> eval_expr state expr.raw )
    | If_else (_, bt, expr_true, expr_false) ->
      let _b, stack = Stack.pop state.stack in
      (* should check b *)
      let state_true =
        eval_instr { state with stack } (Block (None, bt, expr_true))
      in
      let state_false =
        eval_instr { state with stack } (Block (None, bt, expr_false))
      in
      join state_true state_false
    (* | Loop (_str_opt, _bt, expr) ->  *)
    (*     let expr = expr.e in *)
    (*     assert false *)
    | instr -> (
      let res = S.eval_instr state instr in
      match res with
      | S.Ret s -> s
      | Control_flow (state, instr) -> eval_instr state instr )
end

(*===========================================================================*)

module DataState : DATA_STATE = struct
  type t =
    | Ret of state
    | Control_flow of state * instr

  let rec exec_ibinop state size (op : Text.ibinop) : t =
    let e1, e2, stack = Stack.pop_2 state.stack in
    match op with
    | Add ->
      let flags = Flags.Biadd.no_overflow in
      let e =
        Abs_value.binop size
          (ADomain.Binary_Forward.biadd ~flags ~size state.ctx)
          e1 e2
      in
      Ret { state with stack = Stack.push stack e }
    | Sub ->
      let flags = Operator.Flags.Bisub.no_overflow in
      let e =
        Abs_value.binop size
          (ADomain.Binary_Forward.bisub ~flags ~size state.ctx)
          e1 e2
      in
      Ret { state with stack = Stack.push stack e }
    | Mul ->
      let flags = Flags.Bimul.pack ~nsw:true ~nuw:true in
      let e =
        Abs_value.binop size
          (ADomain.Binary_Forward.bimul ~flags ~size state.ctx)
          e1 e2
      in
      Ret { state with stack = Stack.push stack e }
    | Div sx ->
      let op =
        match sx with
        | S -> ADomain.Binary_Forward.bisdiv ~size state.ctx
        | U -> ADomain.Binary_Forward.biudiv ~size state.ctx
      in
      let e = Abs_value.binop size op e1 e2 in
      Ret { state with stack = Stack.push stack e }
    | _ -> assert false

  and eval_instr : state -> instr -> t =
   fun state instr ->
    match instr with
    | I32_const i ->
      let abs_i =
        ADomain.Binary_Forward.biconst ~size:Size.b32 (Z.of_int32 i) state.ctx
      in
      Ret { state with stack = Stack.push state.stack (I32 abs_i) }
    | I64_const i ->
      let abs_i =
        ADomain.Binary_Forward.biconst ~size:Size.b64 (Z.of_int64 i) state.ctx
      in
      Ret { state with stack = Stack.push state.stack (I64 abs_i) }
    | Unreachable -> Fmt.failwith "Unreachable@."
    | I_binop (nn, op) ->
      let size = match nn with Text.S32 -> Size.b32 | Text.S64 -> Size.b64 in
      exec_ibinop state size op
    | Local_get i -> (
      let var = IntMap.find_opt i state.locals in
      match var with
      | Some v -> Ret { state with stack = Stack.push state.stack v }
      | None -> Fmt.failwith "local.get on unset i:%i" i )
    | Local_set i ->
      let e, stack = Stack.pop state.stack in
      Ret { state with stack; locals = IntMap.add i e state.locals }
    | Drop ->
      if Stack.is_empty state.stack then Fmt.failwith "Drop on empty stack"
      else
        let _, stack = Stack.pop state.stack in
        Ret { state with stack }
    | Nop ->
      let top = ADomain.binary_unknown ~size:Size.b32 state.ctx in
      Ret { state with stack = Stack.push state.stack (I32 top) }
    | (If_else _ | Call _ | Block _ | Loop _) as instr ->
      Control_flow (state, instr)
    | instr ->
      Fmt.failwith "DataState.eval_instr not implemented for %a"
        (pp_instr ~short:true) instr
end

module ConcreteFixpoint = DenotFixpoint (DataState)

let expr (link_state : Abs_extern_func.extern_func Link.State.t)
  (m : Abs_extern_func.extern_func Linked.Module.t) =
  let envs = Link.State.get_envs link_state in
  let ctx = ADomain.root_context () in
  let initial_state =
    { ctx
    ; stack = Stack.empty
    ; locals = IntMap.empty
    ; env = m.env
    ; func_rt = []
    ; envs
    }
  in

  List.iter
    begin fun (e : expr Annotated.t) ->
      let end_state = ConcreteFixpoint.eval_expr initial_state e.raw in
      Fmt.pr "End State : %a" pp_state end_state
    end
    m.to_run
