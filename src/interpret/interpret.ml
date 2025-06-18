(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

[@@@ocaml.warning "-32-33"]

open Types
open Binary

(* TODO: make this a CLI parameter *)
let use_ite_for_select = true

module Make (P : Interpret_intf.P) :
  Interpret_intf.S
    with type 'a choice := 'a P.Choice.t
     and type module_to_run := P.Module_to_run.t
     and type thread := P.thread
     and type env := P.Env.t
     and type State.stack := P.Value.t list
     and type value = P.Value.t = struct
  open P
  open Value
  open Choice
  module Stack = Stack.Make [@inlined hint] (Value)

  module I32 = struct
    include I32

    let ( < ) = lt

    let ( <= ) = le

    let ( > ) = gt

    let ( >= ) = ge

    let ( + ) = add

    let ( - ) = sub

    let ( * ) = mul

    let ( / ) = div

    let ( ~- ) x = const_i32 0l - x

    let ( <> ) = ne

    let ( = ) = eq

    let eqz v = v = zero

    let min_int = const_i32 Int32.min_int
  end

  module I64 = struct
    include I64

    let ( < ) = lt

    let ( <= ) = le

    let ( > ) = gt

    let ( >= ) = ge

    let ( + ) = add

    let ( - ) = sub

    let ( * ) = mul

    let ( / ) = div

    let ( <> ) = ne

    let ( = ) = eq

    let eqz v = v = zero

    let min_int = const_i64 Int64.min_int
  end

  let page_size = const_i64 65_536L

  let pop_choice stack =
    let b, stack = Stack.pop_bool stack in
    let* b = select b in
    return (b, stack)

  let ( let> ) v f =
    let* v = select v in
    f v

  let const = const_i32

  let consti i = const_i32 (Int32.of_int i)

  let exec_iunop stack nn op =
    match nn with
    | S32 ->
      let n, stack = Stack.pop_i32 stack in
      let res =
        let open I32 in
        match op with Clz -> clz n | Ctz -> ctz n | Popcnt -> popcnt n
      in
      Stack.push_i32 stack res
    | S64 ->
      let n, stack = Stack.pop_i64 stack in
      let res =
        let open I64 in
        match op with Clz -> clz n | Ctz -> ctz n | Popcnt -> popcnt n
      in
      Stack.push_i64 stack res

  let exec_funop stack nn op =
    match nn with
    | S32 ->
      let open F32 in
      let f, stack = Stack.pop_f32 stack in
      let res =
        match op with
        | Abs -> abs f
        | Neg -> neg f
        | Sqrt -> sqrt f
        | Ceil -> ceil f
        | Floor -> floor f
        | Trunc -> trunc f
        | Nearest -> nearest f
      in
      Stack.push_f32 stack res
    | S64 ->
      let open F64 in
      let f, stack = Stack.pop_f64 stack in
      let res =
        match op with
        | Abs -> abs f
        | Neg -> neg f
        | Sqrt -> sqrt f
        | Ceil -> ceil f
        | Floor -> floor f
        | Trunc -> trunc f
        | Nearest -> nearest f
      in
      Stack.push_f64 stack res

  let exec_ibinop (stack : Stack.t) nn (op : ibinop) : Stack.t Choice.t =
    match nn with
    | S32 ->
      let (n1, n2), stack = Stack.pop2_i32 stack in
      let+ res =
        let open I32 in
        match op with
        | Add -> Choice.return @@ add n1 n2
        | Sub -> Choice.return @@ sub n1 n2
        | Mul -> Choice.return @@ mul n1 n2
        | Div s -> begin
          let> cond = eqz n2 in
          if cond then Choice.trap `Integer_divide_by_zero
          else
            match s with
            | S ->
              let> overflow = Bool.and_ (eq n1 min_int) @@ eq n2 ~-(const 1l) in
              if overflow then Choice.trap `Integer_overflow
              else Choice.return @@ div n1 n2
            | U -> Choice.return @@ unsigned_div n1 n2
        end
        | Rem s -> begin
          let> cond = eqz n2 in
          if cond then Choice.trap `Integer_divide_by_zero
          else
            match s with
            | S -> Choice.return @@ rem n1 n2
            | U -> Choice.return @@ unsigned_rem n1 n2
        end
        | And -> Choice.return @@ logand n1 n2
        | Or -> Choice.return @@ logor n1 n2
        | Xor -> Choice.return @@ logxor n1 n2
        | Shl -> Choice.return @@ shl n1 n2
        | Shr S -> Choice.return @@ shr_s n1 n2
        | Shr U -> Choice.return @@ shr_u n1 n2
        | Rotl -> Choice.return @@ rotl n1 n2
        | Rotr -> Choice.return @@ rotr n1 n2
      in
      Stack.push_i32 stack res
    | S64 ->
      let (n1, n2), stack = Stack.pop2_i64 stack in
      let+ res =
        let open I64 in
        match op with
        | Add -> Choice.return @@ add n1 n2
        | Sub -> Choice.return @@ sub n1 n2
        | Mul -> Choice.return @@ mul n1 n2
        | Div s -> begin
          let> cond = eqz n2 in
          if cond then Choice.trap `Integer_divide_by_zero
          else
            match s with
            | S ->
              let> overflow =
                Bool.and_ (eq n1 min_int)
                @@ eq n2 (sub (const_i64 0L) (const_i64 1L))
              in
              if overflow then Choice.trap `Integer_overflow
              else Choice.return @@ div n1 n2
            | U -> Choice.return @@ unsigned_div n1 n2
        end
        | Rem s -> begin
          let> cond = eqz n2 in
          if cond then Choice.trap `Integer_divide_by_zero
          else
            match s with
            | S -> Choice.return @@ rem n1 n2
            | U -> Choice.return @@ unsigned_rem n1 n2
        end
        | And -> Choice.return @@ logand n1 n2
        | Or -> Choice.return @@ logor n1 n2
        | Xor -> Choice.return @@ logxor n1 n2
        | Shl -> Choice.return @@ shl n1 n2
        | Shr S -> Choice.return @@ shr_s n1 n2
        | Shr U -> Choice.return @@ shr_u n1 n2
        | Rotl -> Choice.return @@ rotl n1 n2
        | Rotr -> Choice.return @@ rotr n1 n2
      in
      Stack.push_i64 stack res

  let exec_fbinop stack nn (op : fbinop) =
    match nn with
    | S32 ->
      let (f1, f2), stack = Stack.pop2_f32 stack in
      Stack.push_f32 stack
        (let open F32 in
         match op with
         | Add -> add f1 f2
         | Sub -> sub f1 f2
         | Mul -> mul f1 f2
         | Div -> div f1 f2
         | Min -> min f1 f2
         | Max -> max f1 f2
         | Copysign -> copy_sign f1 f2 )
    | S64 ->
      let (f1, f2), stack = Stack.pop2_f64 stack in
      Stack.push_f64 stack
        (let open F64 in
         match op with
         | Add -> add f1 f2
         | Sub -> sub f1 f2
         | Mul -> mul f1 f2
         | Div -> div f1 f2
         | Min -> min f1 f2
         | Max -> max f1 f2
         | Copysign -> copy_sign f1 f2 )

  let exec_vibinop stack (shape : ishape) (op : vibinop) =
    let (f1, f2), stack = Stack.pop2_v128 stack in
    let v =
      match shape with
      | I8x16 | I16x8 -> assert false (* TODO *)
      | I32x4 -> begin
        let a1, b1, c1, d1 = V128.to_i32x4 f1 in
        let a2, b2, c2, d2 = V128.to_i32x4 f2 in
        let op = match op with Add -> I32.add | Sub -> I32.sub in
        let a = op a1 a2 in
        let b = op b1 b2 in
        let c = op c1 c2 in
        let d = op d1 d2 in
        V128.of_i32x4 a b c d
      end
      | I64x2 -> begin
        let a1, b1 = V128.to_i64x2 f1 in
        let a2, b2 = V128.to_i64x2 f2 in
        let op = match op with Add -> I64.add | Sub -> I64.sub in
        let a = op a1 a2 in
        let b = op b1 b2 in
        V128.of_i64x2 a b
      end
    in
    Stack.push_v128 stack v

  let exec_itestop stack nn op =
    match nn with
    | S32 ->
      let n, stack = Stack.pop_i32 stack in
      let res = match op with Eqz -> I32.eq_const n 0l in
      Stack.push_bool stack res
    | S64 ->
      let n, stack = Stack.pop_i64 stack in
      let res = match op with Eqz -> I64.eq_const n 0L in
      Stack.push_bool stack res

  let exec_irelop stack nn (op : irelop) =
    match nn with
    | S32 ->
      let (n1, n2), stack = Stack.pop2_i32 stack in
      let res =
        let open I32 in
        match op with
        | Eq -> eq n1 n2
        | Ne -> ne n1 n2
        | Lt S -> lt n1 n2
        | Lt U -> lt_u n1 n2
        | Gt S -> gt n1 n2
        | Gt U -> gt_u n1 n2
        | Le S -> le n1 n2
        | Le U -> le_u n1 n2
        | Ge S -> ge n1 n2
        | Ge U -> ge_u n1 n2
      in
      Stack.push_bool stack res
    | S64 ->
      let (n1, n2), stack = Stack.pop2_i64 stack in
      let res =
        let open I64 in
        match op with
        | Eq -> eq n1 n2
        | Ne -> ne n1 n2
        | Lt S -> lt n1 n2
        | Lt U -> lt_u n1 n2
        | Gt S -> gt n1 n2
        | Gt U -> gt_u n1 n2
        | Le S -> le n1 n2
        | Le U -> le_u n1 n2
        | Ge S -> ge n1 n2
        | Ge U -> ge_u n1 n2
      in
      Stack.push_bool stack res

  let exec_frelop stack nn (op : frelop) =
    match nn with
    | S32 ->
      let (n1, n2), stack = Stack.pop2_f32 stack in
      let res =
        let open F32 in
        match op with
        | Eq -> eq n1 n2
        | Ne -> ne n1 n2
        | Lt -> lt n1 n2
        | Gt -> gt n1 n2
        | Le -> le n1 n2
        | Ge -> ge n1 n2
      in
      Stack.push_bool stack res
    | S64 ->
      let (n1, n2), stack = Stack.pop2_f64 stack in
      let res =
        let open F64 in
        match op with
        | Eq -> eq n1 n2
        | Ne -> ne n1 n2
        | Lt -> lt n1 n2
        | Gt -> gt n1 n2
        | Le -> le n1 n2
        | Ge -> ge n1 n2
      in
      Stack.push_bool stack res

  let exec_itruncf stack nn nn' sx =
    match (nn, nn') with
    | S32, S32 ->
      let f, stack = Stack.pop_f32 stack in
      let res =
        match sx with S -> I32.trunc_f32_s f | U -> I32.trunc_f32_u f
      in
      begin
        match res with
        | Error t -> Choice.trap t
        | Ok res -> Choice.return @@ Stack.push_i32 stack res
      end
    | S32, S64 ->
      let f, stack = Stack.pop_f64 stack in
      let res =
        match sx with S -> I32.trunc_f64_s f | U -> I32.trunc_f64_u f
      in
      begin
        match res with
        | Error t -> Choice.trap t
        | Ok res -> Choice.return @@ Stack.push_i32 stack res
      end
    | S64, S32 ->
      let f, stack = Stack.pop_f32 stack in
      let res =
        match sx with S -> I64.trunc_f32_s f | U -> I64.trunc_f32_u f
      in
      begin
        match res with
        | Error t -> Choice.trap t
        | Ok res -> Choice.return @@ Stack.push_i64 stack res
      end
    | S64, S64 ->
      let f, stack = Stack.pop_f64 stack in
      let res =
        match sx with S -> I64.trunc_f64_s f | U -> I64.trunc_f64_u f
      in
      begin
        match res with
        | Error t -> Choice.trap t
        | Ok res -> Choice.return @@ Stack.push_i64 stack res
      end

  let exec_itruncsatf stack nn nn' sx =
    match nn with
    | S32 -> begin
      match nn' with
      | S32 ->
        let n, stack = Stack.pop_f32 stack in
        let n =
          match sx with
          | S -> I32.trunc_sat_f32_s n
          | U -> I32.trunc_sat_f32_u n
        in
        Stack.push_i32 stack n
      | S64 ->
        let n, stack = Stack.pop_f64 stack in
        let n =
          match sx with
          | S -> I32.trunc_sat_f64_s n
          | U -> I32.trunc_sat_f64_u n
        in
        Stack.push_i32 stack n
    end
    | S64 -> begin
      match nn' with
      | S32 ->
        let n, stack = Stack.pop_f32 stack in
        let n =
          match sx with
          | S -> I64.trunc_sat_f32_s n
          | U -> I64.trunc_sat_f32_u n
        in
        Stack.push_i64 stack n
      | S64 ->
        let n, stack = Stack.pop_f64 stack in
        let n =
          match sx with
          | S -> I64.trunc_sat_f64_s n
          | U -> I64.trunc_sat_f64_u n
        in
        Stack.push_i64 stack n
    end

  let exec_fconverti stack nn nn' sx =
    let is_signed = match sx with S -> true | U -> false in
    match nn with
    | S32 -> (
      let open F32 in
      match nn' with
      | S32 ->
        let n, stack = Stack.pop_i32 stack in
        let n = if is_signed then convert_i32_s n else convert_i32_u n in
        Stack.push_f32 stack n
      | S64 ->
        let n, stack = Stack.pop_i64 stack in
        let n = if is_signed then convert_i64_s n else convert_i64_u n in
        Stack.push_f32 stack n )
    | S64 -> (
      let open F64 in
      match nn' with
      | S32 ->
        let n, stack = Stack.pop_i32 stack in
        let n = if is_signed then convert_i32_s n else convert_i32_u n in
        Stack.push_f64 stack n
      | S64 ->
        let n, stack = Stack.pop_i64 stack in
        let n = if is_signed then convert_i64_s n else convert_i64_u n in
        Stack.push_f64 stack n )

  let exec_ireinterpretf stack nn nn' =
    match nn with
    | S32 -> begin
      match nn' with
      | S32 ->
        let n, stack = Stack.pop_f32 stack in
        let n = I32.reinterpret_f32 n in
        Stack.push_i32 stack n
      | S64 ->
        let n, stack = Stack.pop_f64 stack in
        let n = I32.reinterpret_f32 (F32.demote_f64 n) in
        Stack.push_i32 stack n
    end
    | S64 -> begin
      match nn' with
      | S32 ->
        let n, stack = Stack.pop_f32 stack in
        let n = I64.reinterpret_f64 (F64.promote_f32 n) in
        Stack.push_i64 stack n
      | S64 ->
        let n, stack = Stack.pop_f64 stack in
        let n = I64.reinterpret_f64 n in
        Stack.push_i64 stack n
    end

  let exec_freinterpreti stack nn nn' =
    match nn with
    | S32 -> begin
      match nn' with
      | S32 ->
        let n, stack = Stack.pop_i32 stack in
        let n = F32.reinterpret_i32 n in
        Stack.push_f32 stack n
      | S64 ->
        let n, stack = Stack.pop_i64 stack in
        let n = F32.reinterpret_i32 (I64.to_int32 n) in
        Stack.push_f32 stack n
    end
    | S64 -> begin
      match nn' with
      | S32 ->
        let n, stack = Stack.pop_i32 stack in
        let n = F64.reinterpret_i64 (I64.of_int32 n) in
        Stack.push_f64 stack n
      | S64 ->
        let n, stack = Stack.pop_i64 stack in
        let n = F64.reinterpret_i64 n in
        Stack.push_f64 stack n
    end

  let init_local (_id, t) : Value.t =
    match t with
    | Num_type I32 -> I32 I32.zero
    | Num_type I64 -> I64 I64.zero
    | Num_type F32 -> F32 F32.zero
    | Num_type F64 -> F64 F64.zero
    | Num_type V128 -> V128 V128.zero
    | Ref_type (_null, rt) -> ref_null rt

  (* TODO move to module Env *)
  let mem_0 = 0

  type extern_func = Extern_func.extern_func

  let exec_extern_func env stack (f : extern_func) =
    let pop_arg (type ty) stack (arg : ty Extern_func.telt) :
      (ty * Stack.t) Choice.t =
      match arg with
      | I32 -> Choice.return @@ Stack.pop_i32 stack
      | I64 -> Choice.return @@ Stack.pop_i64 stack
      | F32 -> Choice.return @@ Stack.pop_f32 stack
      | F64 -> Choice.return @@ Stack.pop_f64 stack
      | V128 -> Choice.return @@ Stack.pop_v128 stack
      | Externref ety -> (
        let v, stack = Stack.pop_as_ref stack in
        match Ref.get_externref v ety with
        | Ref_value v -> Choice.return @@ (v, stack)
        | Type_mismatch -> Choice.trap `Extern_call_arg_type_mismatch
        | Null -> Choice.trap `Extern_call_null_arg )
    in
    let rec split_args : type f r.
      Stack.t -> (f, r) Extern_func.atype -> Stack.t * Stack.t =
     fun stack ty ->
      let[@local] split_one_arg args =
        let elt, stack = Stack.pop stack in
        let elts, stack = split_args stack args in
        (elt :: elts, stack)
      in
      match ty with
      | Mem args -> split_args stack args
      | Arg (_, args) -> split_one_arg args
      | UArg args -> split_args stack args
      | NArg (_, _, args) -> split_one_arg args
      | Res -> ([], stack)
    in
    let rec apply : type f r.
      Stack.t -> (f, r) Extern_func.atype -> f -> r Choice.t =
     fun stack ty f ->
      match ty with
      | Mem args ->
        let* mem = Env.get_memory env mem_0 in
        apply stack args (f mem)
      | Arg (arg, args) ->
        let* v, stack = pop_arg stack arg in
        apply stack args (f v)
      | UArg args -> apply stack args (f ())
      | NArg (_, arg, args) ->
        let* v, stack = pop_arg stack arg in
        apply stack args (f v)
      | Res -> Choice.return f
    in
    let (Extern_func.Extern_func (Func (atype, rtype), func)) = f in
    let args, stack = split_args stack atype in
    let* r = apply (List.rev args) atype func in
    let push_val (type ty) (arg : ty Extern_func.telt) (v : ty) stack =
      match arg with
      | I32 -> Stack.push_i32 stack v
      | I64 -> Stack.push_i64 stack v
      | F32 -> Stack.push_f32 stack v
      | F64 -> Stack.push_f64 stack v
      | V128 -> Stack.push_v128 stack v
      | Externref ty -> Stack.push_as_externref stack ty v
    in
    let+ r in
    match (rtype, r) with
    | R0, () -> stack
    | R1 t1, v1 -> push_val t1 v1 stack
    | R2 (t1, t2), (v1, v2) -> push_val t1 v1 stack |> push_val t2 v2
    | R3 (t1, t2, t3), (v1, v2, v3) ->
      push_val t1 v1 stack |> push_val t2 v2 |> push_val t3 v3
    | R4 (t1, t2, t3, t4), (v1, v2, v3, v4) ->
      push_val t1 v1 stack |> push_val t2 v2 |> push_val t3 v3 |> push_val t4 v4

  module State = struct
    type stack = Stack.t

    type value = Value.t

    module Locals : sig
      type t = value array

      val of_list : value list -> t

      val get : t -> int -> value

      val set : t -> int -> value -> t
    end = struct
      type t = value array

      let of_list = Array.of_list

      let get t i = Array.unsafe_get t i

      let set t i v =
        let locals = Array.copy t in
        Array.unsafe_set locals i v;
        locals
    end

    type pc = binary instr list

    type block =
      { branch : pc
      ; branch_rt : binary result_type
      ; continue : pc
      ; continue_rt : binary result_type
      ; stack : stack
      ; is_loop : Prelude.Bool.t
      }

    type block_stack = block list

    type exec_state =
      { return_state : exec_state option
      ; stack : stack
      ; locals : Locals.t
      ; pc : pc
      ; block_stack : block_stack
      ; func_rt : binary result_type
      ; env : Env.t
      ; envs : Env.t Env_id.collection
      }

    let empty_exec_state ~locals ~env ~envs =
      { return_state = None
      ; stack = []
      ; locals = Locals.of_list locals
      ; pc = []
      ; block_stack = []
      ; func_rt = []
      ; env
      ; envs
      }

    type instr_result =
      | Return of value list
      | Continue of exec_state

    let return (state : exec_state) =
      let args = Stack.keep state.stack (List.length state.func_rt) in
      match state.return_state with
      | None -> Return args
      | Some state ->
        let stack = args @ state.stack in
        Continue { state with stack }

    let branch (state : exec_state) n =
      let block_stack = Stack.drop_n state.block_stack n in
      match block_stack with
      | [] -> Choice.return (return state)
      | block :: block_stack_tl ->
        let block_stack =
          if block.is_loop then block_stack else block_stack_tl
        in
        let args = Stack.keep state.stack (List.length block.branch_rt) in
        let stack = args @ block.stack in
        Choice.return
          (Continue { state with block_stack; pc = block.branch; stack })

    let end_block (state : exec_state) =
      match state.block_stack with
      | [] -> Choice.return (return state)
      | block :: block_stack ->
        let args = Stack.keep state.stack (List.length block.continue_rt) in
        let stack = args @ block.stack in
        Choice.return
          (Continue { state with block_stack; pc = block.continue; stack })
  end

  let exec_block (state : State.exec_state) ~is_loop
    (bt : binary block_type option) expr =
    let pt, rt =
      match bt with
      | None -> ([], [])
      | Some (Bt_raw ((None | Some _), (pt, rt))) -> (List.map snd pt, rt)
    in
    let block : State.block =
      let branch_rt, branch = if is_loop then (pt, expr) else (rt, state.pc) in
      { branch
      ; branch_rt
      ; continue = state.pc
      ; continue_rt = rt
      ; stack = Stack.drop_n state.stack (List.length pt)
      ; is_loop
      }
    in
    Choice.return
      (State.Continue
         { state with pc = expr; block_stack = block :: state.block_stack } )

  let exec_func ~return (state : State.exec_state) env (func : binary Types.func)
      =
    Logs.info (fun m ->
      m "calling func  : func %s" (Option.value func.id ~default:"anonymous") );
    let (Bt_raw ((None | Some _), (param_type, result_type))) = func.type_f in
    let args, stack = Stack.pop_n state.stack (List.length param_type) in
    let return_state =
      if return then state.return_state else Some { state with stack }
    in
    let locals =
      State.Locals.of_list @@ List.rev args @ List.map init_local func.locals
    in
    State.
      { stack = []
      ; locals
      ; pc = func.body
      ; block_stack = []
      ; func_rt = result_type
      ; return_state
      ; env
      ; envs = state.envs
      }

  let exec_vfunc ~return (state : State.exec_state) (func : Func_intf.t) =
    match func with
    | WASM (_id, func, env_id) ->
      let env = Env_id.get env_id state.envs in
      Choice.return (State.Continue (exec_func ~return state env func))
    | Extern f ->
      let f = Env.get_extern_func state.env f in
      let+ stack = exec_extern_func state.env state.stack f in
      let state = { state with stack } in
      if return then State.return state else State.Continue state

  let func_type (state : State.exec_state) (f : Func_intf.t) =
    match f with
    | WASM (_, func, _) ->
      let (Bt_raw ((None | Some _), t)) = func.type_f in
      t
    | Extern f ->
      let f = Env.get_extern_func state.env f in
      Extern_func.extern_type f

  let call_ref ~return:_ (_state : State.exec_state) _typ_i =
    (* TODO *)
    assert false
  (* let fun_ref, stack = Stack.pop_as_ref state.stack in *)
  (* let state = { state with stack } in *)
  (* let func = *)
  (*   match fun_ref with *)
  (*   | exception Invalid_argument _ -> trap "undefined element" *)
  (*   | Funcref (Some f) -> f *)
  (*   | Funcref None -> trap (Printf.sprintf "calling null function reference") *)
  (*   | _ -> trap "element type error" *)
  (* in *)
  (* let pt, rt = Func.typ func in *)
  (* let pt', rt' = typ_i in *)
  (* if not (rt = rt' && List.equal p_type_eq pt pt') then *)
  (*   trap "indirect call type mismatch"; *)
  (* exec_vfunc ~return state func *)

  let call_indirect ~return (state : State.exec_state)
    (tbl_i, (Bt_raw ((None | Some _), typ_i) : binary block_type)) =
    let fun_i, stack = Stack.pop_i32 state.stack in
    let state = { state with stack } in
    let* t = Env.get_table state.env tbl_i in
    let _null, ref_kind = Table.typ t in
    match ref_kind with
    | Func_ht ->
      let size = Table.size t in
      let> out_of_bounds = I32.(le_u (consti size) fun_i) in
      if out_of_bounds then Choice.trap `Undefined_element
      else
        let* fun_i = Choice.select_i32 fun_i in
        let fun_i = Int32.to_int fun_i in
        let f_ref = Table.get t fun_i in
        begin
          match Ref.get_func f_ref with
          | Null -> Choice.trap (`Uninitialized_element fun_i)
          | Type_mismatch -> Choice.trap `Element_type_error
          | Ref_value func ->
            let ft = func_type state func in
            let ft' = typ_i in
            if not (Types.func_type_eq ft ft') then
              Choice.trap `Indirect_call_type_mismatch
            else exec_vfunc ~return state func
        end
    | _ -> Choice.trap `Indirect_call_type_mismatch

  let exec_instr instr (state : State.exec_state) : State.instr_result Choice.t
      =
    let stack = state.stack in
    let env = state.env in
    let locals = state.locals in
    let st stack = Choice.return (State.Continue { state with stack }) in
    Logs.info (fun m -> m "stack         : [ %a ]" Stack.pp stack);
    Logs.info (fun m ->
      m "running instr : %a" (Types.pp_instr ~short:true) instr );
    let* () =
      match Logs.level () with
      | Some Logs.Debug ->
        let+ pc = Choice.get_pc () in
        Logs.debug (fun m ->
          m "path condition: [ %a ]" Smtml.Expr.pp_list
            (Smtml.Expr.Set.to_list pc) )
      | None | Some _ -> return ()
    in
    match instr with
    | Return -> Choice.return (State.return state)
    | Nop -> Choice.return (State.Continue state)
    | Unreachable -> Choice.trap `Unreachable
    | I32_const n -> st @@ Stack.push_const_i32 stack n
    | I64_const n -> st @@ Stack.push_const_i64 stack n
    | F32_const f -> st @@ Stack.push_const_f32 stack f
    | F64_const f -> st @@ Stack.push_const_f64 stack f
    | V128_const f -> st @@ Stack.push_const_v128 stack f
    | I_unop (nn, op) -> st @@ exec_iunop stack nn op
    | F_unop (nn, op) -> st @@ exec_funop stack nn op
    | I_binop (nn, op) ->
      let* stack = exec_ibinop stack nn op in
      st stack
    | F_binop (nn, op) -> st @@ exec_fbinop stack nn op
    | V_ibinop (shape, op) -> st @@ exec_vibinop stack shape op
    | I_testop (nn, op) -> st @@ exec_itestop stack nn op
    | I_relop (nn, op) -> st @@ exec_irelop stack nn op
    | F_relop (nn, op) -> st @@ exec_frelop stack nn op
    | I_extend8_s nn -> begin
      match nn with
      | S32 ->
        let n, stack = Stack.pop_i32 stack in
        let n = I32.extend_s 8 n in
        st @@ Stack.push_i32 stack n
      | S64 ->
        let n, stack = Stack.pop_i64 stack in
        let n = I64.extend_s 8 n in
        st @@ Stack.push_i64 stack n
    end
    | I_extend16_s nn -> begin
      match nn with
      | S32 ->
        let n, stack = Stack.pop_i32 stack in
        let n = I32.extend_s 16 n in
        st @@ Stack.push_i32 stack n
      | S64 ->
        let n, stack = Stack.pop_i64 stack in
        let n = I64.extend_s 16 n in
        st @@ Stack.push_i64 stack n
    end
    | I64_extend32_s ->
      let n, stack = Stack.pop_i64 stack in
      let n = I64.extend_s 32 n in
      st @@ Stack.push_i64 stack n
    | I32_wrap_i64 ->
      let n, stack = Stack.pop_i64 stack in
      let n = I32.wrap_i64 n in
      st @@ Stack.push_i32 stack n
    | I64_extend_i32 s ->
      let n, stack = Stack.pop_i32 stack in
      let n =
        match s with S -> I64.extend_i32_s n | U -> I64.extend_i32_u n
      in
      st @@ Stack.push_i64 stack n
    | I_trunc_f (nn, nn', s) ->
      let* stack = exec_itruncf stack nn nn' s in
      st stack
    | I_trunc_sat_f (nn, nn', s) -> st @@ exec_itruncsatf stack nn nn' s
    | F32_demote_f64 ->
      let n, stack = Stack.pop_f64 stack in
      let n = F32.demote_f64 n in
      st @@ Stack.push_f32 stack n
    | F64_promote_f32 ->
      let n, stack = Stack.pop_f32 stack in
      let n = F64.promote_f32 n in
      st @@ Stack.push_f64 stack n
    | F_convert_i (nn, nn', s) -> st @@ exec_fconverti stack nn nn' s
    | I_reinterpret_f (nn, nn') -> st @@ exec_ireinterpretf stack nn nn'
    | F_reinterpret_i (nn, nn') -> st @@ exec_freinterpreti stack nn nn'
    | Ref_null t -> st @@ Stack.push stack (ref_null t)
    | Ref_is_null ->
      let r, stack = Stack.pop_as_ref stack in
      let is_null = ref_is_null r in
      st @@ Stack.push_bool stack is_null
    | Ref_func (Raw i) ->
      let f = Env.get_func env i in
      st @@ Stack.push stack (ref_func f)
    | Drop -> st @@ Stack.drop stack
    | Local_get (Raw i) -> st @@ Stack.push stack (State.Locals.get locals i)
    | Local_set (Raw i) ->
      let v, stack = Stack.pop stack in
      let locals = State.Locals.set locals i v in
      Choice.return (State.Continue { state with locals; stack })
    | If_else (_id, bt, e1, e2) ->
      let* b, stack = pop_choice stack in
      let state = { state with stack } in
      exec_block state ~is_loop:false bt (if b then e1 else e2)
    | Call (Raw i) -> begin
      let func = Env.get_func env i in
      exec_vfunc ~return:false state func
    end
    | Return_call (Raw i) -> begin
      let func = Env.get_func env i in
      exec_vfunc ~return:true state func
    end
    | Br (Raw i) -> State.branch state i
    | Br_if (Raw i) ->
      let* b, stack = pop_choice stack in
      let state = { state with stack } in
      if b then State.branch state i else Choice.return (State.Continue state)
    | Loop (_id, bt, e) -> exec_block state ~is_loop:true bt e
    | Block (_id, bt, e) -> exec_block state ~is_loop:false bt e
    | Memory_size ->
      let* mem = Env.get_memory env mem_0 in
      let len = Memory.size_in_pages mem in
      st @@ Stack.push_i32 stack len
    | Memory_grow -> begin
      let* mem = Env.get_memory env mem_0 in
      let old_size = I64.of_int32 @@ Memory.size mem in
      let max_size = Memory.get_limit_max mem in
      let delta, stack = Stack.pop_i32 stack in
      let delta = I64.(of_int32 delta * page_size) in
      let new_size = I64.(old_size + delta) in
      let> too_big =
        Bool.or_ I64.(ge_u new_size (page_size * page_size))
        @@
        match max_size with
        | Some max -> I64.(gt_u new_size (max * page_size))
        | None ->
          (* TODO: replace by false... *)
          I64.(const_i64 0L <> const_i64 0L)
      in
      st
      @@
      if too_big then Stack.push_i32 stack I32.(sub (const 0l) (const 1l))
      else begin
        Memory.grow mem I64.(to_int32 delta);
        let res = I64.(to_int32 @@ (old_size / page_size)) in
        Stack.push_i32 stack res
      end
    end
    | Memory_fill ->
      let len, stack = Stack.pop_i32 stack in
      let c, stack = Stack.pop_i32 stack in
      let pos, stack = Stack.pop_i32 stack in
      let* c = Choice.select_i32 c in
      let c =
        let c = Int32.to_int c in
        let c = Int.abs c mod 256 in
        Char.chr c
      in
      (* TODO: move out of bonds check here ! *)
      let* mem = Env.get_memory env mem_0 in
      let> out_of_bounds = Memory.fill mem ~pos ~len c in
      if out_of_bounds then Choice.trap `Out_of_bounds_memory_access
      else st stack
    | Memory_copy ->
      let len, stack = Stack.pop_i32 stack in
      let src, stack = Stack.pop_i32 stack in
      let dst, stack = Stack.pop_i32 stack in
      (* TODO: move out of bonds check here ! *)
      let* mem = Env.get_memory env mem_0 in
      let> out_of_bounds = Memory.blit mem ~src ~dst ~len in
      if out_of_bounds then Choice.trap `Out_of_bounds_memory_access
      else st stack
    | Memory_init (Raw i) ->
      let len, stack = Stack.pop_i32 stack in
      let src, stack = Stack.pop_i32 stack in
      let dst, stack = Stack.pop_i32 stack in
      let* data = Env.get_data env i in
      let data = Data.value data in
      (* TODO: move out of bonds check here ! *)
      let* mem = Env.get_memory env mem_0 in
      let> out_of_bounds = Memory.blit_string mem data ~src ~dst ~len in
      if out_of_bounds then Choice.trap `Out_of_bounds_memory_access
      else st stack
    | Select _t ->
      if use_ite_for_select then begin
        let b, stack = Stack.pop_bool stack in
        let o2, stack = Stack.pop stack in
        let o1, stack = Stack.pop stack in
        let* res = P.select b ~if_true:o1 ~if_false:o2 in
        st @@ Stack.push stack res
      end
      else begin
        let* b, stack = pop_choice stack in
        let o2, stack = Stack.pop stack in
        let o1, stack = Stack.pop stack in
        st @@ Stack.push stack (if b then o1 else o2)
      end
    | Local_tee (Raw i) ->
      let v, stack = Stack.pop stack in
      let locals = State.Locals.set locals i v in
      let stack = Stack.push stack v in
      Choice.return (State.Continue { state with locals; stack })
    | Global_get (Raw i) ->
      let* g = Env.get_global env i in
      st @@ Stack.push stack (Global.value g)
    | Global_set (Raw i) ->
      let* global = Env.get_global env i in
      let v, stack =
        match Global.typ global with
        | Ref_type _rt -> Stack.pop_ref stack
        | Num_type nt -> (
          match nt with
          | I32 ->
            let v, stack = Stack.pop_i32 stack in
            (I32 v, stack)
          | I64 ->
            let v, stack = Stack.pop_i64 stack in
            (I64 v, stack)
          | F32 ->
            let v, stack = Stack.pop_f32 stack in
            (F32 v, stack)
          | F64 ->
            let v, stack = Stack.pop_f64 stack in
            (F64 v, stack)
          | V128 ->
            let v, stack = Stack.pop_v128 stack in
            (V128 v, stack) )
      in
      Global.set_value global v;
      st stack
    | Table_get (Raw i) ->
      let* t = Env.get_table env i in
      let i, stack = Stack.pop_i32 stack in
      let* i = Choice.select_i32 i in
      let i = Int32.to_int i in
      let size = Table.size t in
      if i < 0 || i >= size then Choice.trap `Out_of_bounds_table_access
      else
        let v = Table.get t i in
        st @@ Stack.push stack (Ref v)
    | Table_set (Raw indice) ->
      let* t = Env.get_table env indice in
      let v, stack = Stack.pop_as_ref stack in
      let indice, stack = Stack.pop_i32 stack in
      let* indice = Choice.select_i32 indice in
      let indice = Int32.to_int indice in
      if indice < 0 || indice >= Table.size t then
        Choice.trap `Out_of_bounds_table_access
      else begin
        Table.set t indice v;
        st stack
      end
    | Table_size (Raw indice) ->
      let* t = Env.get_table env indice in
      let size = consti @@ Table.size t in
      st @@ Stack.push_i32 stack size
    | Table_grow (Raw indice) ->
      let* t = Env.get_table env indice in
      let size = consti @@ Table.size t in
      let delta, stack = Stack.pop_i32 stack in
      let new_size = I32.(size + delta) in
      let allowed =
        Bool.and_
          ( match Table.max_size t with
          | None -> Bool.const true
          | Some max -> I32.ge_u (consti max) new_size )
          (I32.ge_u new_size size)
      in
      let> allowed in
      if not allowed then
        let stack = Stack.drop stack in
        st @@ Stack.push_i32_of_int stack (-1)
      else
        let new_element, stack = Stack.pop_as_ref stack in
        let* new_size = Choice.select_i32 new_size in
        Table.grow t new_size new_element;
        st @@ Stack.push_i32 stack size
    | Table_fill (Raw indice) ->
      let* t = Env.get_table env indice in
      let len, stack = Stack.pop_i32 stack in
      let x, stack = Stack.pop_as_ref stack in
      let pos, stack = Stack.pop_i32 stack in
      let> out_of_bounds = I32.gt_u I32.(pos + len) (consti (Table.size t)) in
      if out_of_bounds then Choice.trap `Out_of_bounds_table_access
      else begin
        let* pos = Choice.select_i32 pos in
        let* len = Choice.select_i32 len in
        Table.fill t pos len x;
        st stack
      end
    | Table_copy (Raw ti_dst, Raw ti_src) -> begin
      let* t_src = Env.get_table env ti_src in
      let* t_dst = Env.get_table env ti_dst in
      let len, stack = Stack.pop_i32 stack in
      let src, stack = Stack.pop_i32 stack in
      let dst, stack = Stack.pop_i32 stack in
      let> out_of_bounds =
        let t_src_len = Table.size t_src in
        let t_dst_len = Table.size t_dst in
        Bool.or_ (I32.gt_u I32.(src + len) (consti t_src_len))
        @@ Bool.or_ (I32.gt_u I32.(dst + len) (consti t_dst_len))
        (* TODO: I don't understand why this last one check is needed... *)
        @@ Bool.or_ (I32.lt src (const 0l)) (I32.lt dst (const 0l))
      in
      if out_of_bounds then Choice.trap `Out_of_bounds_table_access
      else begin
        let* () =
          let> len_is_not_zero = I32.ne len (const 0l) in
          if len_is_not_zero then begin
            let* src = Choice.select_i32 src in
            let* dst = Choice.select_i32 dst in
            let+ len = Choice.select_i32 len in
            Table.copy ~t_src ~t_dst ~src ~dst ~len
          end
          else return ()
        in
        st stack
      end
    end
    | Table_init (Raw t_i, Raw e_i) -> begin
      let* t = Env.get_table env t_i in
      let elem = Env.get_elem env e_i in
      let len, stack = Stack.pop_i32 stack in
      let pos_x, stack = Stack.pop_i32 stack in
      let pos, stack = Stack.pop_i32 stack in

      let table_size = Table.size t in
      let elem_len = Elem.size elem in
      let> out_of_bounds =
        Bool.or_ I32.(gt_u (pos_x + len) (consti elem_len))
        @@ Bool.or_
             I32.(gt_u (pos + len) (consti table_size))
             I32.(const 0l > pos)
      in
      if out_of_bounds then Choice.trap `Out_of_bounds_table_access
      else begin
        let* len = Choice.select_i32 len in
        let* pos_x = Choice.select_i32 pos_x in
        let* pos = Choice.select_i32 pos in
        let len = Int32.to_int len in
        let pos_x = Int32.to_int pos_x in
        let pos = Int32.to_int pos in
        for i = 0 to len - 1 do
          let elt = Elem.get elem (pos_x + i) in
          Table.set t (pos + i) elt
        done;
        st stack
      end
    end
    | Elem_drop (Raw i) ->
      let elem = Env.get_elem env i in
      Env.drop_elem elem;
      st stack
    | I_load16 (nn, sx, { offset; _ }) -> (
      let* mem = Env.get_memory env mem_0 in
      let pos, stack = Stack.pop_i32 stack in
      let offset = const offset in
      let addr = I32.(pos + offset) in
      let> out_of_bounds =
        Bool.or_ I32.(offset < const 0l)
        @@ Bool.or_
             I32.(lt_u (Memory.size mem) (addr + const 2l))
             I32.(pos < const 0l)
      in
      if out_of_bounds then Choice.trap `Out_of_bounds_memory_access
      else
        let* mem = Env.get_memory env mem_0 in
        let* res =
          (match sx with S -> Memory.load_16_s | U -> Memory.load_16_u)
            mem addr
        in
        st
        @@
        match nn with
        | S32 -> Stack.push_i32 stack res
        | S64 -> Stack.push_i64 stack (I64.of_int32 res) )
    | I_load8 (nn, sx, { offset; _ }) -> (
      let* mem = Env.get_memory env mem_0 in
      let pos, stack = Stack.pop_i32 stack in
      let offset = const offset in
      let addr = I32.(pos + offset) in
      let> out_of_bounds =
        Bool.or_ I32.(offset < const 0l)
        @@ Bool.or_
             I32.(lt_u (Memory.size mem) (addr + const 1l))
             I32.(pos < const 0l)
      in
      if out_of_bounds then Choice.trap `Out_of_bounds_memory_access
      else
        let* mem = Env.get_memory env mem_0 in
        let* res =
          (match sx with S -> Memory.load_8_s | U -> Memory.load_8_u) mem addr
        in
        st
        @@
        match nn with
        | S32 -> Stack.push_i32 stack res
        | S64 -> Stack.push_i64 stack (I64.of_int32 res) )
    | I_store8 (nn, { offset; _ }) ->
      let* mem = Env.get_memory env mem_0 in
      let n, stack =
        match nn with
        | S32 ->
          let n, stack = Stack.pop_i32 stack in
          (n, stack)
        | S64 ->
          let n, stack = Stack.pop_i64 stack in
          (I64.to_int32 n, stack)
      in
      let pos, stack = Stack.pop_i32 stack in
      let offset = const offset in
      let addr = I32.(pos + offset) in
      let> out_of_bounds =
        Bool.or_ I32.(offset < const 0l)
        @@ Bool.or_
             I32.(lt_u (Memory.size mem) (addr + const 1l))
             I32.(pos < const 0l)
      in
      if out_of_bounds then Choice.trap `Out_of_bounds_memory_access
      else begin
        let* mem = Env.get_memory env mem_0 in
        let* () = Memory.store_8 mem ~addr n in
        (* Thread memory ? *)
        st stack
      end
    | I_load (nn, { offset; _ }) ->
      let* mem = Env.get_memory env mem_0 in
      let pos, stack = Stack.pop_i32 stack in
      let memory_length = Memory.size mem in
      let offset = const offset in
      let addr = I32.(pos + offset) in
      let> out_of_bounds =
        Bool.or_ I32.(offset < const 0l) I32.(pos < const 0l)
      in
      if out_of_bounds then Choice.trap `Out_of_bounds_memory_access
      else begin
        match nn with
        | S32 ->
          let> out_of_bounds = I32.(lt_u memory_length (addr + const 4l)) in
          if out_of_bounds then Choice.trap `Out_of_bounds_memory_access
          else
            let* mem = Env.get_memory env mem_0 in
            let* res = Memory.load_32 mem addr in
            st @@ Stack.push_i32 stack res
        | S64 ->
          let> out_of_bounds = I32.(lt_u memory_length (addr + const 8l)) in
          if out_of_bounds then Choice.trap `Out_of_bounds_memory_access
          else
            let* mem = Env.get_memory env mem_0 in
            let* res = Memory.load_64 mem addr in
            st @@ Stack.push_i64 stack res
      end
    | F_load (nn, { offset; _ }) ->
      let* mem = Env.get_memory env mem_0 in
      let pos, stack = Stack.pop_i32 stack in
      let memory_length = Memory.size mem in
      let offset = const offset in
      let addr = I32.(pos + offset) in
      let> out_of_bounds =
        Bool.or_ I32.(offset < const 0l) @@ I32.(pos < const 0l)
      in
      if out_of_bounds then Choice.trap `Out_of_bounds_memory_access
      else begin
        match nn with
        | S32 ->
          let> out_of_bounds = I32.(lt_u memory_length (addr + const 4l)) in
          if out_of_bounds then Choice.trap `Out_of_bounds_memory_access
          else
            let* mem = Env.get_memory env mem_0 in
            let* res = Memory.load_32 mem addr in
            let res = F32.of_bits res in
            st @@ Stack.push_f32 stack res
        | S64 ->
          let> out_of_bounds = I32.(lt_u memory_length (addr + const 8l)) in
          if out_of_bounds then Choice.trap `Out_of_bounds_memory_access
          else
            let* mem = Env.get_memory env mem_0 in
            let* res = Memory.load_64 mem addr in
            let res = F64.of_bits res in
            st @@ Stack.push_f64 stack res
      end
    | I_store (nn, { offset; _ }) -> (
      let* mem = Env.get_memory env mem_0 in
      let memory_length = Memory.size mem in
      let offset = const offset in
      match nn with
      | S32 ->
        let n, stack = Stack.pop_i32 stack in
        let pos, stack = Stack.pop_i32 stack in
        let addr = I32.(pos + offset) in
        let> out_of_bounds =
          Bool.or_ I32.(lt_u memory_length (addr + const 4l))
          @@ I32.(pos < const 0l)
        in
        if out_of_bounds then Choice.trap `Out_of_bounds_memory_access
        else begin
          let* mem = Env.get_memory env mem_0 in
          let* () = Memory.store_32 mem ~addr n in
          st stack
        end
      | S64 ->
        let n, stack = Stack.pop_i64 stack in
        let pos, stack = Stack.pop_i32 stack in
        let addr = I32.(pos + offset) in
        let> out_of_bounds =
          Bool.or_ I32.(lt_u memory_length (addr + const 8l))
          @@ I32.(pos < const 0l)
        in
        if out_of_bounds then Choice.trap `Out_of_bounds_memory_access
        else begin
          let* mem = Env.get_memory env mem_0 in
          let* () = Memory.store_64 mem ~addr n in
          st stack
        end )
    | F_store (nn, { offset; _ }) -> (
      let* mem = Env.get_memory env mem_0 in
      let memory_length = Memory.size mem in
      let offset = const offset in
      match nn with
      | S32 ->
        let n, stack = Stack.pop_f32 stack in
        let pos, stack = Stack.pop_i32 stack in
        let addr = I32.(pos + offset) in
        let> out_of_bounds =
          Bool.or_ I32.(lt_u memory_length (addr + const 4l))
          @@ I32.(pos < const 0l)
        in
        if out_of_bounds then Choice.trap `Out_of_bounds_memory_access
        else begin
          let* mem = Env.get_memory env mem_0 in
          let* () = Memory.store_32 mem ~addr (F32.to_bits n) in
          st stack
        end
      | S64 ->
        let n, stack = Stack.pop_f64 stack in
        let pos, stack = Stack.pop_i32 stack in
        let addr = I32.(pos + offset) in
        let> out_of_bounds =
          Bool.or_ I32.(lt_u memory_length (addr + const 8l))
          @@ I32.(pos < const 0l)
        in
        if out_of_bounds then Choice.trap `Out_of_bounds_memory_access
        else begin
          let* mem = Env.get_memory env mem_0 in
          let* () = Memory.store_64 mem ~addr (F64.to_bits n) in
          st stack
        end )
    | I64_load32 (sx, { offset; _ }) ->
      let* mem = Env.get_memory env mem_0 in
      let offset = const offset in
      let memory_length = Memory.size mem in
      let pos, stack = Stack.pop_i32 stack in
      let addr = I32.(pos + offset) in
      let> out_of_bounds =
        Bool.or_ I32.(offset < const 0l)
        @@ Bool.or_ I32.(pos < const 0l)
        @@ I32.(lt_u memory_length (addr + const 4l))
      in
      if out_of_bounds then Choice.trap `Out_of_bounds_memory_access
      else begin
        let* mem = Env.get_memory env mem_0 in
        let* res = Memory.load_32 mem addr in
        let res = I64.of_int32 res in
        let res =
          match sx with
          | S -> res
          | U ->
            let open I64 in
            let a = shl (const_i64 1L) (const_i64 32L) in
            let b = a - const_i64 1L in
            logand res b
        in
        st @@ Stack.push_i64 stack res
      end
    | I_store16 (nn, { offset; _ }) ->
      let* mem = Env.get_memory env mem_0 in
      let offset = const offset in
      let memory_length = Memory.size mem in
      let n, stack =
        match nn with
        | S32 ->
          let n, stack = Stack.pop_i32 stack in
          (n, stack)
        | S64 ->
          let n, stack = Stack.pop_i64 stack in
          (I64.to_int32 n, stack)
      in
      let pos, stack = Stack.pop_i32 stack in
      let addr = I32.(pos + offset) in
      let> out_of_bounds =
        Bool.or_ I32.(pos < const 0l) I32.(lt_u memory_length (addr + const 2l))
      in
      if out_of_bounds then Choice.trap `Out_of_bounds_memory_access
      else begin
        let* mem = Env.get_memory env mem_0 in
        let* () = Memory.store_16 mem ~addr n in
        st stack
      end
    | I64_store32 { offset; _ } ->
      let* mem = Env.get_memory env mem_0 in
      let offset = const offset in
      let memory_length = Memory.size mem in
      let n, stack = Stack.pop_i64 stack in
      let n = I64.to_int32 n in
      let pos, stack = Stack.pop_i32 stack in
      let addr = I32.(pos + offset) in
      let> out_of_bounds =
        Bool.or_ I32.(pos < const 0l)
        @@ I32.(lt_u memory_length (addr + const 4l))
      in
      if out_of_bounds then Choice.trap `Out_of_bounds_memory_access
      else begin
        let* mem = Env.get_memory env mem_0 in
        let* () = Memory.store_32 mem ~addr n in
        st stack
      end
    | Data_drop (Raw i) ->
      let* data = Env.get_data env i in
      Env.drop_data data;
      st stack
    | Br_table (inds, Raw i) ->
      let target, stack = Stack.pop_i32 stack in
      let> out = I32.(ge_u target (const (Int32.of_int (Array.length inds)))) in
      let* target =
        if out then return i
        else
          let+ target = Choice.select_i32 target in
          let target = Int32.to_int target in
          let (Raw i) = inds.(target) in
          i
      in
      let state = { state with stack } in
      State.branch state target
    | Call_indirect (Raw tbl_i, typ_i) ->
      call_indirect ~return:false state (tbl_i, typ_i)
    | Return_call_indirect (Raw tbl_i, typ_i) ->
      call_indirect ~return:true state (tbl_i, typ_i)
    | Call_ref typ_i -> call_ref ~return:false state typ_i
    | Return_call_ref typ_i -> call_ref ~return:true state typ_i
    | (Extern_externalize | Extern_internalize) as i ->
      Logs.err (fun m ->
        m "unimplemented instruction: %a" (Types.pp_instr ~short:false) i );
      assert false

  let rec loop ~heartbeat (state : State.exec_state) =
    let* () =
      match heartbeat with None -> Choice.return () | Some f -> f ()
    in
    match state.pc with
    | instr :: pc -> begin
      let* state = exec_instr instr { state with pc } in
      match state with
      | State.Continue state -> loop ~heartbeat state
      | State.Return res -> Choice.return res
    end
    | [] -> (
      let* next_state = State.end_block state in
      match next_state with
      | State.Continue state -> loop ~heartbeat state
      | State.Return res -> Choice.return res )

  let exec_expr ~heartbeat envs env locals stack expr bt =
    let state : State.exec_state =
      let func_rt = match bt with None -> [] | Some rt -> rt in
      { stack
      ; locals
      ; env
      ; envs
      ; func_rt
      ; block_stack = []
      ; pc = expr
      ; return_state = None
      }
    in

    let+ state = loop ~heartbeat state in
    state

  let make_heartbeat ~timeout ~timeout_instr () =
    match (timeout, timeout_instr) with
    | None, None -> None
    | Some _, _ | _, Some _ ->
      Some
        (let fuel =
           Atomic.make (match timeout_instr with Some i -> i | None -> max_int)
         in
         let after_time =
           let start_time = Unix.gettimeofday () in
           fun timeout_s ->
             Float.compare (Unix.gettimeofday () -. start_time) timeout_s > 0
         in
         fun () ->
           let fuel_left = Atomic.fetch_and_add fuel (-1) in
           (* If we only use [timeout_instr], we want to stop all as
              soon as [fuel_left <= 0]. But if we only use [timeout],
              we don't want to run into the slow path below on each
              instruction after [fuel_left] becomes negative. We avoid
              this repeated slow path by bumping [fuel] to [max_int]
              again in this case. *)
           if fuel_left mod 1024 = 0 || fuel_left < 0 then begin
             let stop =
               match (timeout, timeout_instr) with
               | None, None -> assert false
               | None, Some _instr -> fuel_left <= 0
               | Some s, Some _instr -> after_time s || fuel_left <= 0
               | Some s, None ->
                 let stop = after_time s in
                 if (not stop) && fuel_left < 0 then Atomic.set fuel max_int;
                 stop
             in
             if stop then Choice.trap (`Msg "timeout") else Choice.return ()
           end
           else Choice.return () )

  let modul ~timeout ~timeout_instr envs (modul : Module_to_run.t) :
    unit P.Choice.t =
    let heartbeat = make_heartbeat ~timeout ~timeout_instr () in
    Logs.info (fun m -> m "interpreting ...");
    try
      begin
        let* () =
          List.fold_left
            (fun u to_run ->
              let* () = u in
              let+ _end_stack =
                let env = Module_to_run.env modul in
                exec_expr ~heartbeat envs env (State.Locals.of_list [])
                  Stack.empty to_run None
              in
              () )
            (Choice.return ())
            (Module_to_run.to_run modul)
        in
        Choice.return ()
      end
    with Stack_overflow -> Choice.trap `Call_stack_exhausted

  let exec_vfunc_from_outside ~locals ~env ~envs func : _ list Choice.t =
    let env = Env_id.get env envs in
    let exec_state = State.empty_exec_state ~locals ~env ~envs in
    try
      begin
        let* state =
          match func with
          | Func_intf.WASM (_id, func, env_id) ->
            let env = Env_id.get env_id exec_state.State.envs in
            let stack = locals in
            let state = State.{ exec_state with stack } in
            Choice.return
              (State.Continue (exec_func ~return:true state env func))
          | Extern f ->
            let f = Env.get_extern_func exec_state.env f in
            let+ stack = exec_extern_func exec_state.env exec_state.stack f in
            let state = State.{ exec_state with stack } in
            State.return state
        in
        match state with
        | State.Return res -> Choice.return res
        | State.Continue state ->
          let+ res = loop ~heartbeat:None state in
          res
      end
    with Stack_overflow -> Choice.trap `Call_stack_exhausted

  type value = Value.t
end

module Concrete = Make [@inlined hint] (Concrete)
module Symbolic = Make [@inlined hint] (Symbolic)
module Minimalist_symbolic = Make [@inlined hint] (Minimalist_symbolic)
module Concolic = Make [@inlined hint] (Concolic)
