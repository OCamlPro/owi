open Binary
open Abs_value
open Result.Syntax

type value = Domain.integer

type stack = value list

type state =
  { ctx : Domain.Context.t
  ; stack : stack
  ; locals : value array
  ; func_rt : val_type list
  ; env : Abs_extern_func.extern_func Link_env.t
  ; envs : Abs_extern_func.extern_func Link_env.t Dynarray.t
  }

let pp_state : Format.formatter -> state -> unit =
 fun fmt state ->
  Fmt.pf fmt "{@[<v 2>@;stack : %a,@;locals : %a;@]@\n}"
    (Fmt.list ~sep:Fmt.semi (Domain.integer_pretty state.ctx))
    state.stack
    (Fmt.array ~sep:Fmt.semi (Domain.integer_pretty state.ctx))
    state.locals

let stack_pop : 'a list -> value * 'a list = function
  | [] -> Fmt.failwith "TODO"
  | h :: t -> (h, t)

let stack_pop_n : 'a list -> int -> 'a list * 'a list =
 fun list i -> (List.take i list, List.drop i list)

let serialize ~widens : state -> state -> (state, 'a) Domain.Context.result =
 fun state_a state_b ->
  let rec serialize_lists lhs rhs result =
    match (lhs, rhs) with
    | [], [] -> result
    | [], _ | _, [] -> Fmt.failwith "join on stacks of different sizes"
    | i1 :: rest_a, i2 :: rest_b ->
      let (Domain.Context.Result (included, in_tuple, continue)) = result in
      if Domain.Integer.equal i1 i2 then serialize_lists rest_a rest_b result
      else
        let (Domain.Context.Result (included, in_tuple, local_continue)) =
          Domain.serialize_integer ~widens state_a.ctx i1 state_b.ctx i2
            (included, in_tuple)
        in
        let new_result =
          Domain.Context.Result
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
      (Domain.Context.Result
         (true, Domain.Context.empty_tuple (), fun _ctx out -> ([], out)) )
  in
  let (Domain.Context.Result (included, in_tuple, locals_continue)) =
    locals_result
  in

  let stack_result =
    serialize_lists state_a.stack state_b.stack
      (Domain.Context.Result (included, in_tuple, fun _ctx out -> ([], out)))
  in

  let (Domain.Context.Result (included, in_tuple, stack_continue)) =
    stack_result
  in
  Domain.Context.Result
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
  let (Domain.Context.Result (_inc, in_tuple, continue)) =
    serialize ~widens:false state_a state_b
  in
  let ctx, out = Domain.typed_nondet2 state_a.ctx state_b.ctx in_tuple in
  fst @@ continue ctx out

let join_opt a b =
  match (a, b) with None, x | x, None -> x | Some a, Some b -> Some (join a b)

let rec exec_func (state : state) (func : Func.t) =
  Log.info (fun m ->
    m "calling func  : func %s" (Option.value func.id ~default:"anonymous") );
  let (Bt_raw ((None | Some _), (param_type, result_type))) = func.type_f in
  let args, caller_popped_stack =
    stack_pop_n state.stack (List.length param_type)
  in
  let fn_state =
    { state with
      stack = []
    ; ctx = Domain.Context.copy state.ctx
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

and exec_extern_func stack (f : Abs_extern_func.extern_func) =
  let rec get_arrity : type f r. (f, r) Abs_extern_func.atype -> int = function
    | Mem (_, args) -> 1 + get_arrity args
    | Arg (_, args) -> 1 + get_arrity args
    | UArg _ -> 1
    | NArg (_, _, args) -> 1 + get_arrity args
    | Res -> 0
  in
  let pop_arg (type ty) stack (arg : ty Abs_extern_func.telt) : ty * stack =
    match arg with
    | I32 -> stack_pop stack
    | I64 -> stack_pop stack
    | _ -> assert false
  in
  let rec apply : type f r. stack -> (f, r) Abs_extern_func.atype -> f -> r =
   fun stack ty f ->
    match ty with
    | Mem (_, _) -> Fmt.failwith "no mem args for now >:{"
    | Arg (arg, args) ->
      let v, stack = pop_arg stack arg in
      apply stack args (f v)
    | UArg args -> apply stack args (f ())
    | NArg (_, arg, args) ->
      let v, stack = pop_arg stack arg in
      apply stack args (f v)
    | Res -> f
  in
  let (Abs_extern_func.Extern_func (Func (atype, _rtype), func)) = f in
  let args, _stack = stack_pop_n stack (get_arrity atype) in
  let+ _r = apply (List.rev args) atype func in
  assert false
(* match (rtype, r) with *)
(* | R0, () -> stack *)
(* | R1 _, v1 -> v1 :: stack *)
(* | _ -> assert false *)

and exec_ibinop state : Text.ibinop -> state = function
  | Add ->
    let e1, stack = stack_pop state.stack in
    let e2, stack = stack_pop stack in
    let e = Domain.Integer_Forward.iadd state.ctx e1 e2 in
    { state with stack = e :: stack }
  | Sub ->
    let e1, stack = stack_pop state.stack in
    let e2, stack = stack_pop stack in
    let e = Domain.Integer_Forward.isub state.ctx e1 e2 in
    { state with stack = e :: stack }
  | Mul ->
    let e1, stack = stack_pop state.stack in
    let e2, stack = stack_pop stack in
    let e = Domain.Integer_Forward.imul state.ctx e1 e2 in
    { state with stack = e :: stack }
  | Div _sx ->
    let e1, stack = stack_pop state.stack in
    let e2, stack = stack_pop stack in
    let e = Domain.Integer_Forward.idiv state.ctx e1 e2 in
    { state with stack = e :: stack }
  | _ -> assert false

and exec_instr state : instr -> state = function
  | I32_const i ->
    let abs_i = Domain.Integer_Forward.iconst (Z.of_int32 i) state.ctx in
    { state with stack = abs_i :: state.stack }
  | I64_const i ->
    let abs_i = Domain.Integer_Forward.iconst (Z.of_int64 i) state.ctx in
    { state with stack = abs_i :: state.stack }
  | Unreachable -> Fmt.failwith "unreachable"
  | I_binop (_nn, op) -> exec_ibinop state op
  | If_else (_, _, expr_true, expr_false) ->
    let _b, stack = stack_pop state.stack in
    (* should check b *)
    let state_true = expr { state with stack } expr_true.raw in
    let state_false = expr { state with stack } expr_false.raw in
    join state_true state_false
  | Return -> (
    match state.func_rt with
    | [] -> state
    | rt ->
      let return_values, _ = stack_pop_n state.stack (List.length rt) in
      assert (List.length return_values = List.length state.func_rt);
      Fmt.pr "returning %a@\n"
        (Fmt.list ~sep:(Fmt.any ",") (Domain.integer_pretty state.ctx))
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
      let stack = exec_extern_func state.stack f in
      begin match stack with
      | Error err -> Fmt.failwith "Error : %s@\n" (Result.err_to_string err)
      | Ok stack -> { state with stack }
      end
    end
  | Local_get i -> { state with stack = state.locals.(i) :: state.stack }
  | Local_set i ->
    let e, stack = stack_pop state.stack in
    state.locals.(i) <- e;
    { state with stack }
  | Drop ->
    { state with stack = Domain.Integer_Forward.one state.ctx :: state.stack }
  | instr -> Fmt.failwith "Instr unimplemented %a" (pp_instr ~short:true) instr

and expr (state : state) (expr : expr) : state =
  List.fold_left
    (fun acc (x : instr Annotated.t) -> exec_instr acc x.raw)
    state expr

let expr (link_state : 'extern_func Link.State.t)
  (m : 'extern_func Linked.Module.t) =
  let envs = Link.State.get_envs link_state in
  let ctx = Domain.root_context () in
  let initial_state =
    { ctx
    ; stack = []
    ; locals = Array.init 100 (fun _ -> Domain.Integer_Forward.zero ctx)
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
