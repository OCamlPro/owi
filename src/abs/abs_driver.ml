(* interger -> bitvector *)
(* C smith - generator des programme C *)
(* C vise/C reduce - genere des prog plus petit pour trouver les bugs *)
(* fromalisation *)
(* Instantation du domaine pour la mémoire *)

open Binary
open Abs_value
open Abs_datastructures

type value = Abs_value.t

type state =
  { ctx : Context.t
  ; stack : Stack.t
  ; locals : value array
  ; func_rt : val_type list
  ; env : Abs_extern_func.extern_func Link_env.t
  ; envs : Abs_extern_func.extern_func Link_env.t Dynarray.t
  }

let size_s32 = Units.In_bits.s32

module Flags = Operator.Flags

let pp_state : Format.formatter -> state -> unit =
 fun fmt state ->
  (* Fmt.pf fmt "{@[<v 2>@;stack : %a,@;locals : %a;@]@\n}" *)
  Fmt.pf fmt "{@[<v 2>@;stack : %a,@]@\n}"
    (Fmt.list ~sep:Fmt.semi (pp state.ctx))
    state.stack
    (* (Fmt.array ~sep:Fmt.semi (AbsDomain.binary_pretty ~size:size_s32 state.ctx)) *)
    (* state.locals *)

let serialize ~widens : state -> state -> (state, 'a) AbsDomain.Context.result =
 fun state_a state_b ->
  let rec serialize_lists lhs rhs result =
    match (lhs, rhs) with
    | [], [] -> result
    | [], _ | _, [] -> Fmt.failwith "join on stacks of different sizes"
    | i1 :: rest_a, i2 :: rest_b ->
      let (AbsDomain.Context.Result (included, in_tuple, continue)) = result in
      if AbsDomain.Binary.equal i1 i2 then serialize_lists rest_a rest_b result
      else
        let (AbsDomain.Context.Result (included, in_tuple, local_continue)) =
          AbsDomain.serialize_binary ~size:size_s32 ~widens state_a.ctx i1
            state_b.ctx i2 (included, in_tuple)
        in
        let new_result =
          AbsDomain.Context.Result
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
      (Array.to_list state_a.locals)
      (Array.to_list state_b.locals)
      (AbsDomain.Context.Result
         (true, AbsDomain.Context.empty_tuple (), fun _ctx out -> ([], out)) )
  in
  let (AbsDomain.Context.Result (included, in_tuple, locals_continue)) =
    locals_result
  in

  let stack_result =
    serialize_lists state_a.stack state_b.stack
      (AbsDomain.Context.Result (included, in_tuple, fun _ctx out -> ([], out)))
  in

  let (AbsDomain.Context.Result (included, in_tuple, stack_continue)) =
    stack_result
  in
  AbsDomain.Context.Result
    ( included
    , in_tuple
    , fun ctx out ->
        let stack, out = stack_continue ctx out in
        let locals, out = locals_continue ctx out in
        ( { state_a with
            ctx
          ; stack = List.rev stack
          ; locals = Array.of_list @@ List.rev locals
          }
        , out ) )

let join state_a state_b =
  let (AbsDomain.Context.Result (_inc, in_tuple, continue)) =
    serialize ~widens:false state_a state_b
  in
  let ctx, out = AbsDomain.typed_nondet2 state_a.ctx state_b.ctx in_tuple in
  fst @@ continue ctx out

let join_opt a b =
  match (a, b) with None, x | x, None -> x | Some a, Some b -> Some (join a b)

let exec_extern_func state (f : Abs_extern_func.extern_func) =
  let stack = state.stack in
  let pop_arg (type ty) stack (arg : ty Abs_extern_func.telt) : ty * Stack.t =
    match arg with I32 -> Stack.pop stack | _ -> assert false
  in
  let rec split_args : type f r.
    'stack -> (f, r) Abs_extern_func.atype -> 'stack * 'stack =
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
  let rec apply : type f r. Stack.t -> (f, r) Abs_extern_func.atype -> f -> r =
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
    let r = apply (List.rev args) atype func in
    match (rtype, r) with
    | R0, Ok () -> stack
    | R1 I32, Ok _v1 ->
      AbsDomain.binary_unknown ~size:Units.In_bits.s32 state.ctx :: stack
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
      stack = []
    ; ctx = AbsDomain.Context.copy state.ctx
    ; func_rt = result_type
    ; locals = Array.of_list args
    }
  in
  Log.info (fun m -> m "Func start state : %a" pp_state fn_state);
  let func_end_state = expr fn_state func.body.raw in
  Log.info (fun m -> m "Func end state : %a" pp_state func_end_state);
  { state with
    stack =
      caller_popped_stack
      @ List.take (List.length result_type) func_end_state.stack
  }

and exec_ibinop state : Text.ibinop -> state = function
  | Add ->
    let e1, stack = Stack.pop state.stack in
    let e2, stack = Stack.pop stack in
    let e =
      AbsDomain.Binary_Forward.biadd state.ctx e1 e2 ~size:size_s32
        ~flags:Flags.Biadd.no_overflow
    in
    { state with stack = e :: stack }
  | Sub ->
    let e1, stack = Stack.pop state.stack in
    let e2, stack = Stack.pop stack in
    let e =
      AbsDomain.Binary_Forward.bisub state.ctx e1 e2 ~size:size_s32
        ~flags:Operator.Flags.Bisub.no_overflow
    in
    { state with stack = e :: stack }
  | Mul ->
    let e1, stack = Stack.pop state.stack in
    let e2, stack = Stack.pop stack in
    let e =
      AbsDomain.Binary_Forward.bimul state.ctx e1 e2 ~size:size_s32
        ~flags:(Flags.Bimul.pack ~nsw:false ~nuw:false)
    in
    { state with stack = e :: stack }
  | Div _sx ->
    let e1, stack = Stack.pop state.stack in
    let e2, stack = Stack.pop stack in
    let e = AbsDomain.Binary_Forward.bisdiv ~size:size_s32 state.ctx e1 e2 in
    { state with stack = e :: stack }
  | _ -> assert false

and exec_instr state : instr -> state = function
  | I32_const i ->
    let abs_i =
      AbsDomain.Binary_Forward.biconst ~size:size_s32 (Z.of_int32 i) state.ctx
    in
    { state with stack = abs_i :: state.stack }
  | I64_const i ->
    let abs_i =
      AbsDomain.Binary_Forward.biconst ~size:size_s32 (Z.of_int64 i) state.ctx
    in
    { state with stack = abs_i :: state.stack }
  | Unreachable -> Fmt.failwith "unreachable"
  | I_binop (_nn, op) -> exec_ibinop state op
  | If_else (_, _, expr_true, expr_false) ->
    let _b, stack = Stack.pop state.stack in
    (* should check b *)
    let state_true = expr { state with stack } expr_true.raw in
    let state_false = expr { state with stack } expr_false.raw in
    join state_true state_false
  | Return -> (
    match state.func_rt with
    | [] ->
      (* Fmt.pr "Returning in root with state %a" pp_state state; *)
      state
    | rt ->
      let return_values, _ = Stack.pop_n state.stack (List.length rt) in
      assert (List.length return_values = List.length state.func_rt);
      Fmt.pr "returning %a@\n"
        (Fmt.list ~sep:(Fmt.any ",")
           (AbsDomain.binary_pretty ~size:size_s32 state.ctx) )
        return_values;
      { state with stack = return_values } )
  | Call idx ->
    let func = Link_env.get_func state.env idx in
    begin match func with
    | Wasm { func; idx } ->
      let env = Dynarray.get state.envs idx in
      exec_func { state with env } func
    | Extern _ ->
      let f = Link_env.get_extern_func state.env idx in
      let stack = exec_extern_func state f in
      { state with stack }
    end
  | Local_get i -> { state with stack = state.locals.(i) :: state.stack }
  | Local_set i ->
    let e, stack = Stack.pop state.stack in
    state.locals.(i) <- e;
    { state with stack }
  | Drop -> (
    Fmt.pr "dropping, stack : %a"
      (Fmt.list ~sep:(Fmt.any ",") AbsDomain.Binary.pretty)
      state.stack;
    match state.stack with
    | [] -> Fmt.failwith "Drop on empty stack"
    | _ :: t -> { state with stack = t } )
  | instr -> Fmt.failwith "Instr unimplemented %a" (pp_instr ~short:true) instr

and expr (state : state) (expr : expr) : state =
  List.fold_left
    (fun acc (x : instr Annotated.t) -> exec_instr acc x.raw)
    state expr

let expr (link_state : Abs_extern_func.extern_func Link.State.t)
  (m : Abs_extern_func.extern_func Linked.Module.t) =
  let envs = Link.State.get_envs link_state in
  let ctx = AbsDomain.root_context () in
  let initial_state =
    { ctx
    ; stack = []
    ; locals =
        Array.init 10 (fun _ -> AbsDomain.binary_unknown ~size:size_s32 ctx)
    ; env = m.env
    ; func_rt = []
    ; envs
    }
  in

  List.iter
    begin fun (e : expr Annotated.t) ->
      let _end_state = expr initial_state e.raw in
      ()
      (* Fmt.pr "End State : %a" pp_state end_state *)
    end
    m.to_run
