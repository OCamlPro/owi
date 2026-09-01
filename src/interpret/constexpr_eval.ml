(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

module Make (Value : Value_intf.T) :
  Constexpr_eval_intf.T
    with type value := Value.t
     and type reference := Value.t Value.Ref.t
     and type context := unit = struct
  module Stack = Stack.Make [@inlined hint] (Value)

  let default_gc_val : Binary.storage_type -> Value.t = function
    | Val_type (Num_type I32) -> I32 (Value.I32.of_int32 0l)
    | Val_type (Num_type I64) -> I64 (Value.I64.of_int64 0L)
    | Val_type (Num_type F32) -> F32 (Value.F32.of_bits (Value.I32.of_int32 0l))
    | Val_type (Num_type F64) -> F64 (Value.F64.of_bits (Value.I64.of_int64 0L))
    | Val_type (Num_type V128) -> V128 Value.V128.zero
    | Val_type (Ref_type (_, ht)) -> Ref (Value.Ref.null ht)
    | Pack_type _ -> I32 (Value.I32.of_int32 0l)

  let i32_instr stack : Binary.i32_instr -> _ = function
    | Const i -> Stack.push_i32 stack (Value.I32.of_int32 i)
    | Add -> Stack.apply_i32_i32_i32 stack Value.I32.add
    | Sub -> Stack.apply_i32_i32_i32 stack Value.I32.sub
    | Mul -> Stack.apply_i32_i32_i32 stack Value.I32.mul
    | _ -> assert false

  let i64_instr stack : Binary.i64_instr -> _ = function
    | Const i -> Stack.push_i64 stack (Value.I64.of_int64 i)
    | Add -> Stack.apply_i64_i64_i64 stack Value.I64.add
    | Sub -> Stack.apply_i64_i64_i64 stack Value.I64.sub
    | Mul -> Stack.apply_i64_i64_i64 stack Value.I64.mul
    | _ -> assert false

  let simple_instruction ~get_const_type ~get_const_func ~get_const_global stack
      = function
    | Binary.I32 i -> (i32_instr stack i)
    | Binary.I64 i -> (i64_instr stack i)
    | F32 (Const f) ->
       Stack.push_f32 stack (Value.F32.of_float32 f)
    | F64 (Const f) ->
      Stack.push_f64 stack (Value.F64.of_float (Float64.to_float f))
    | V128 (Const v) ->
       Stack.push_v128 stack (Value.V128.of_concrete v)
    | Ref (Null t) ->  Stack.push_ref stack (Value.Ref.null t)
    | Ref (Func id) ->
      let f = get_const_func id in
      let value = Value.Ref (Func (Some f)) in
       Stack.push stack value
    | Global (Get id) ->
      let g = get_const_global id in
       Stack.push stack g
    | Array (New id) ->
      let n, stack = Stack.pop_i32 stack in
      let v, stack = Stack.pop stack in
      let a = Value.Ref.Array (Value.Ref.Array.new_fill id v n) in
       Stack.push_ref stack a
    | Array (New_default id) ->
      let n, stack = Stack.pop_i32 stack in
      let typ : Binary.sub_type = get_const_type id in
      let st =
        match typ.ct with
        | Def_array_t (_, st) -> st
        | _ ->
          (* typechecking ensures this can not happen *)
          assert false
      in
      let a =
        Value.Ref.Array (Value.Ref.Array.new_fill id (default_gc_val st) n)
      in
       Stack.push_ref stack a
    | Array (New_fixed (id, n)) ->
      let n = Int32.to_int n in
      let top_n, stack = Stack.pop_n stack n in
      let elems = Array.of_list (List.rev top_n) in
      let a = Value.Ref.Array (Value.Ref.Array.new_fixed_with id elems) in
       Stack.push_ref stack a
    | I31 Ref ->
      let n, stack = Stack.pop_i32 stack in
       Stack.push_ref stack (Value.Ref.make_i31 n)
    | Struct (New id) ->
      let typ : Binary.sub_type = get_const_type id in
      let fields =
        match typ.ct with
        | Def_struct_t fl -> fl
        | _ ->
          (* typechecking ensures this can not happen *)
          assert false
      in
      let n = List.length fields in
      let top_n, stack = Stack.pop_n stack n in
      let s =
        Value.Ref.Struct
          (Value.Ref.Struct.new_with id (Array.of_list (List.rev top_n)))
      in
       Stack.push_ref stack s
    | Struct (New_default id) ->
      let typ : Binary.sub_type = get_const_type id in
      let fields =
        match typ.ct with
        | Def_struct_t fl -> fl
        | _ -> (* typechecking ensures this can not happen *) assert false
      in
      let defaults =
        Array.of_list (List.map (fun (_, (_, st)) -> default_gc_val st) fields)
      in
      let s = Value.Ref.Struct (Value.Ref.Struct.new_with id defaults) in
       Stack.push_ref stack s
    | Any_convert_extern ->
      let r, stack = Stack.pop_as_ref stack in
       Stack.push_ref stack (Value.Ref.any_convert_extern r)
    | Extern_convert_any ->
      let r, stack = Stack.pop_as_ref stack in
       Stack.push_ref stack (Value.Ref.extern_convert_any r)
    | F32 _ | F64 _ | V128 _ | Global _ | Local _ | Ref _ | Drop | Nop
    | Unreachable | I8x16 _ | I16x8 _ | I32x4 _ | I64x2 _ | F32x4 _ | F64x2 _
    | Table _ | Elem _ | Memory _ | Data _ | I31 _ | Struct _ | Array _
    | Select _ ->
      assert false

  let instr ~get_const_type ~get_const_func ~get_const_global stack instr =
    match instr.Annotated.raw with
    | Binary.Simple i ->
      simple_instruction ~get_const_type ~get_const_func ~get_const_global stack
        i
    | _ ->
      (* typechecking ensures this can not happen *)
      assert false

  (* TODO: the modul parameter can probably be removed *)
  let expr _ctx ~get_const_type ~get_const_func ~get_const_global
    (e : Binary.expr) : Value.t =
      let stack =
      List.fold_left
        (instr ~get_const_type ~get_const_func ~get_const_global)
        Stack.empty e
    in
    match stack with
    | []
    | _ :: _ :: _ ->
      (* typechecking ensures this can not happen *)
        assert false
    | [ result ] -> result

  let ref_expr ctx ~get_const_type ~get_const_func ~get_const_global
    (e : Binary.expr) : Value.t Value.Ref.t =
    match expr ctx ~get_const_type ~get_const_func ~get_const_global e with
    | (Ref v) -> v
    | _ -> assert false
end

module Concrete = Make (Concrete_value)
module Symbolic = Make (Symbolic_value)

module Abstract :
  Constexpr_eval_intf.T
    with type value := Abstract_value.t
     and type reference := Abstract_value.t Abstract_ref.t
     and type context := Abstract_domain.Context.t = struct
  module Value = Abstract_value
  module Stack = Abstract_stack

  let default_gc_val _ = assert false

  let i32_instr ctx stack : Binary.i32_instr -> _ = function
    | Const i -> Stack.push_i32 stack (Value.I32.of_int32 ctx i)
    | Add -> Stack.apply_i32_i32_i32 stack (Value.I32.add ctx)
    | Sub -> Stack.apply_i32_i32_i32 stack (Value.I32.sub ctx)
    | Mul -> Stack.apply_i32_i32_i32 stack (Value.I32.mul ctx)
    | _ -> assert false

  let i64_instr ctx stack : Binary.i64_instr -> _ = function
    | Const i -> Stack.push_i64 stack (Value.I64.of_int64 ctx i)
    | Add -> Stack.apply_i64_i64_i64 stack (Value.I64.add ctx)
    | Sub -> Stack.apply_i64_i64_i64 stack (Value.I64.sub ctx)
    | Mul -> Stack.apply_i64_i64_i64 stack (Value.I64.mul ctx)
    | _ -> assert false

  let simple_instruction ctx ~get_const_type:_ ~get_const_func ~get_const_global
    stack = function
    | Binary.I32 i ->  (i32_instr ctx stack i)
    | Binary.I64 i ->  (i64_instr ctx stack i)
    | F32 (Const f) ->
       Stack.push_f32 stack (Value.F32.of_float32 ctx f)
    | F64 (Const f) ->
       Stack.push_f64 stack (Value.F64.of_float ctx (Float64.to_float f))
    | V128 (Const v) ->
       Stack.push_v128 stack (Value.V128.of_concrete ctx v)
    | Ref (Null t) ->  Stack.push_ref stack (Value.Ref.null ctx t)
    | Ref (Func id) ->
      let f = get_const_func id in
      let value = Value.Ref (Func (Some f)) in
       Stack.push stack value
    | Global (Get id) ->
      let g = get_const_global id in
       Stack.push stack g
    | _ -> assert false

  let instr ctx ~get_const_type ~get_const_func ~get_const_global stack instr =
    match instr.Annotated.raw with
    | Binary.Simple i ->
      simple_instruction ctx ~get_const_type ~get_const_func ~get_const_global
        stack i
    | _ -> assert false

  (* TODO: the modul parameter can probably be removed *)
  let expr ctx ~get_const_type ~get_const_func ~get_const_global
    (e : Binary.expr) : Value.t =
    let stack =
      List.fold_left
        (instr ctx ~get_const_type ~get_const_func ~get_const_global)
        Stack.empty e
    in
    match stack with
    | []
    | _ :: _ :: _ ->
      (* typechecking *)
      assert false
    | [ result ] ->  result

  let ref_expr ctx ~get_const_type ~get_const_func ~get_const_global
    (e : Binary.expr) : Value.t Value.Ref.t  =
    match expr ctx ~get_const_type ~get_const_func ~get_const_global e with
    |  (Ref v) ->  v
    |  _ -> assert false
end
