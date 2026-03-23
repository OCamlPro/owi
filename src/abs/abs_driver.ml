open Binary
open Abs_value
open Abs_datastructures

type value = Abs_value.ADomain.binary

type state =
  { ctx : ADomain.Context.t
  ; stack : value Stack.t
  ; locals : value Dynarray.t
  ; func_rt : val_type list
  ; env : Abs_extern_func.extern_func Link_env.t
  ; envs : Abs_extern_func.extern_func Link_env.t Dynarray.t
  }

module Flags = Operator.Flags

let pp_value : ADomain.Context.t -> Format.formatter -> value -> unit =
 fun ctx fmt v ->
  (*don't know what to do about this size*)
  Fmt.pf fmt "%a" (ADomain.binary_pretty ~size:Size.b32 ctx) v

let pp_state : Format.formatter -> state -> unit =
 fun fmt state ->
  Fmt.pf fmt "{@\n@[<hov 2>  ctx : %a,@;stack : %a,@;locals : %a@]@\n}"
    ADomain.context_pretty state.ctx
    (Stack.pp @@ pp_value state.ctx)
    state.stack
    (Fmt.array ~sep:Fmt.semi (pp_value state.ctx))
    (Dynarray.to_array state.locals)

let serialize ~widens : state -> state -> (state, 'a) ADomain.Context.result =
 fun state_a state_b ->
  let rec serialize_lists lhs rhs result =
    match (lhs, rhs) with
    | [], [] -> result
    | [], _ | _, [] -> Fmt.failwith "join on stacks of different sizes"
    | i1 :: rest_a, i2 :: rest_b ->
      let (ADomain.Context.Result (included, in_tuple, continue)) = result in
      if ADomain.Binary.equal i1 i2 then serialize_lists rest_a rest_b result
      else
        let (ADomain.Context.Result (included, in_tuple, local_continue)) =
          ADomain.serialize_binary ~size:Size.b32 ~widens state_a.ctx i1
            state_b.ctx i2 (included, in_tuple)
        in
        let new_result =
          ADomain.Context.Result
            ( included
            , in_tuple
            , fun ctx out_tuple ->
                let integer, out_tuple = local_continue ctx out_tuple in
                let stack, out_tuple = continue ctx out_tuple in
                (integer :: stack, out_tuple) )
        in
        serialize_lists rest_a rest_b new_result
  in
  let locals_result =
    serialize_lists
      (Dynarray.to_list state_a.locals)
      (Dynarray.to_list state_b.locals)
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
          ; locals = Dynarray.of_list @@ List.rev locals
          }
        , out ) )

let join state_a state_b =
  let (ADomain.Context.Result (_inc, in_tuple, continue)) =
    serialize ~widens:false state_a state_b
  in
  let ctx, out = ADomain.typed_nondet2 state_a.ctx state_b.ctx in_tuple in
  fst @@ continue ctx out

let join_opt a b =
  match (a, b) with None, x | x, None -> x | Some a, Some b -> Some (join a b)

let exec_extern_func stack (f : Abs_extern_func.extern_func) =
  let pop_arg (type ty) (stack : value Stack.t) (arg : ty Abs_extern_func.telt)
    : ty * value Stack.t =
    match arg with I32 -> Stack.pop stack | _ -> assert false
  in
  let rec split_args : type f r.
    'stack -> (f, r) Abs_extern_func.atype -> 'stack * 'stack =
   fun stack ty ->
    let[@local] split_one_arg args =
      let elt, stack = Stack.pop stack in
      let elts, stack = split_args stack args in
      (Stack.push elts elt, stack)
    in
    match ty with
    | Mem (_, args) -> split_args stack args
    | Arg (_, args) -> split_one_arg args
    | UArg args -> split_args stack args
    | NArg (_, _, args) -> split_one_arg args
    | Res -> (Stack.empty, stack)
  in
  let rec apply : type f r.
    value Stack.t -> (f, r) Abs_extern_func.atype -> f -> r =
   fun stack ty f ->
    match ty with
    | Arg (arg, args) ->
      let v, stack = pop_arg stack arg in
      apply stack args (f v)
    | UArg args -> apply stack args (f ())
    | NArg (_, arg, args) ->
      let v, stack = pop_arg stack arg in
      apply stack args (f v)
    | Res -> f
    | Mem (_memid, _args) -> assert false
  in
  match f with
  | Abs_extern_func.Extern_func (Abs_extern_func.Func (atype, rtype), func) -> (
    let args, stack = split_args stack atype in
    let r = apply (Stack.rev args) atype func in
    match (rtype, r) with
    | R0, Ok () -> stack
    | R1 I32, Ok v1 -> Stack.push stack v1
    | _ -> assert false )

let rec exec_func (state : state) (func : Func.t) =
  Log.info (fun m ->
    m "calling func  : func %s" (Option.value func.id ~default:"anonymous") );
  let (Bt_raw ((None | Some _), (param_type, result_type))) = func.type_f in
  let args, caller_popped_stack =
    Stack.pop_n state.stack (List.length param_type)
  in
  let fn_state =
    { state with
      stack = Stack.empty
    ; ctx = state.ctx
    ; func_rt = result_type
    ; locals = Dynarray.of_list (Stack.to_list args)
    }
  in
  Log.info (fun m -> m "Func start state : %a" pp_state fn_state);
  let func_end_state = exec_expr fn_state func.body.raw in
  (* We should probably copy state and join back the return values in the context here *)
  Log.info (fun m -> m "Func end state : %a@." pp_state func_end_state);
  { state with
    stack =
      Stack.append caller_popped_stack
      @@ Stack.take (List.length result_type) func_end_state.stack
  }

and exec_ibinop state size : Text.ibinop -> state = function
  | Add ->
    let e1, stack = Stack.pop state.stack in
    let e2, stack = Stack.pop stack in
    let e =
      ADomain.Binary_Forward.biadd state.ctx e1 e2 ~size
        ~flags:Flags.Biadd.no_overflow
    in
    { state with stack = Stack.push stack e }
  | Sub ->
    let e1, stack = Stack.pop state.stack in
    let e2, stack = Stack.pop stack in
    let e =
      ADomain.Binary_Forward.bisub state.ctx e1 e2 ~size
        ~flags:Operator.Flags.Bisub.no_overflow
    in
    { state with stack = Stack.push stack e }
  | Mul ->
    let e1, stack = Stack.pop state.stack in
    let e2, stack = Stack.pop stack in
    let e =
      ADomain.Binary_Forward.bimul state.ctx e1 e2 ~size
        ~flags:(Flags.Bimul.pack ~nsw:false ~nuw:false)
    in
    { state with stack = Stack.push stack e }
  | Div _sx ->
    let e1, stack = Stack.pop state.stack in
    let e2, stack = Stack.pop stack in
    let e = ADomain.Binary_Forward.bisdiv ~size state.ctx e1 e2 in
    { state with stack = Stack.push stack e }
  | _ -> assert false

and exec_instr state : instr -> state = function
  | I32_const i ->
    let abs_i =
      ADomain.Binary_Forward.biconst ~size:Size.b32 (Z.of_int32 i) state.ctx
    in
    { state with stack = Stack.push state.stack abs_i }
  | I64_const i ->
    let abs_i =
      ADomain.Binary_Forward.biconst ~size:Size.b64 (Z.of_int64 i) state.ctx
    in
    { state with stack = Stack.push state.stack abs_i }
  | Unreachable -> Fmt.failwith "unreachable"
  | I_binop (nn, op) ->
    let size = match nn with Text.S32 -> Size.b32 | Text.S64 -> Size.b64 in
    exec_ibinop state size op
  | Block (_, bt, expr) -> (
    match bt with
    | Some (Bt_raw (_, ft)) ->
      let param_type, result_type = ft in
      let block_stack = Stack.take (List.length param_type) state.stack in
      let res = exec_expr { state with stack = block_stack } expr.raw in
      let stack = Stack.take (List.length result_type) res.stack in
      { state with stack = Stack.append state.stack stack; locals = res.locals }
    | None -> exec_expr state expr.raw )
  | If_else (_, bt, expr_true, expr_false) ->
    let _b, stack = Stack.pop state.stack in
    (* should check b *)
    let state_true =
      exec_instr { state with stack } (Block (None, bt, expr_true))
    in
    let state_false =
      exec_instr { state with stack } (Block (None, bt, expr_false))
    in
    join state_true state_false
  | Return -> (
    match state.func_rt with
    | [] ->
      (* Fmt.pr "Returning in root with state %a" pp_state state; *)
      state
    | rt ->
      let return_values, _ = Stack.pop_n state.stack (List.length rt) in
      assert (
        List.length (Stack.to_list return_values) = List.length state.func_rt );
      Fmt.pr "returning %a@\n" (Stack.pp @@ pp_value state.ctx) state.stack;
      { state with stack = return_values } )
  | Call idx ->
    let func = Link_env.get_func state.env idx in
    begin match func with
    | Wasm { func; idx } ->
      let env = Dynarray.get state.envs idx in
      exec_func { state with env } func
    | Extern _ ->
      let f = Link_env.get_extern_func state.env idx in
      let stack = exec_extern_func state.stack f in
      { state with stack }
    end
  | Local_get i ->
    let var = Dynarray.get state.locals i in
    { state with stack = Stack.push state.stack var }
  | Local_set i ->
    let e, stack = Stack.pop state.stack in
    Dynarray.set state.locals i e;
    { state with stack }
  | Drop ->
    Fmt.pr "dropping, stack : %a" (Stack.pp @@ pp_value state.ctx) state.stack;
    if Stack.is_empty state.stack then Fmt.failwith "Drop on empty stack"
    else
      let _, stack = Stack.pop state.stack in
      { state with stack }
  | Nop ->
    let top = ADomain.binary_unknown ~size:Size.b32 state.ctx in
    { state with stack = Stack.push state.stack top }
  | instr -> Fmt.failwith "Instr unimplemented %a" (pp_instr ~short:true) instr

and exec_expr (state : state) (expr : expr) : state =
  List.fold_left
    (fun acc (x : instr Annotated.t) -> exec_instr acc x.raw)
    state expr

let expr (link_state : Abs_extern_func.extern_func Link.State.t)
  (m : Abs_extern_func.extern_func Linked.Module.t) =
  let envs = Link.State.get_envs link_state in
  let ctx = ADomain.root_context () in
  let initial_state =
    { ctx
    ; stack = Stack.empty
    ; locals = Dynarray.of_list []
    ; env = m.env
    ; func_rt = []
    ; envs
    }
  in

  List.iter
    begin fun (e : expr Annotated.t) ->
      let _end_state = exec_expr initial_state e.raw in
      ()
      (* Fmt.pr "End State : %a" pp_state end_state *)
    end
    m.to_run
