(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax

module Make (Value : Value_intf.T) :
  Constexpr_eval_intf.T
    with type value := Value.t
     and type reference := Value.t Value.Ref.t
     and type context := unit = struct
  module Stack = Stack.Make [@inlined hint] (Value)

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

  let simple_instruction ~get_const_func ~get_const_global stack = function
    | Binary.I32 i -> Result.ok (i32_instr stack i)
    | Binary.I64 i -> Result.ok (i64_instr stack i)
    | F32 (Const f) ->
      Result.ok @@ Stack.push_f32 stack (Value.F32.of_float32 f)
    | F64 (Const f) ->
      Result.ok
      @@ Stack.push_f64 stack (Value.F64.of_float (Float64.to_float f))
    | V128 (Const v) ->
      Result.ok @@ Stack.push_v128 stack (Value.V128.of_concrete v)
    | Ref (Null t) -> Result.ok @@ Stack.push_ref stack (Value.Ref.null t)
    | Ref (Func id) ->
      let* f = get_const_func id in
      let value = Value.Ref (Func (Some f)) in
      Result.ok @@ Stack.push stack value
    | Global (Get id) ->
      let* g = get_const_global id in
      Result.ok @@ Stack.push stack g
    | _ -> assert false

  let instr ~get_const_func ~get_const_global stack instr =
    match instr.Annotated.raw with
    | Binary.Simple i ->
      simple_instruction ~get_const_func ~get_const_global stack i
    | _ -> assert false

  (* TODO: the modul parameter can probably be removed *)
  let expr _ctx ~get_const_func ~get_const_global (e : Binary.expr) :
    Value.t Result.t =
    let* stack =
      list_fold_left (instr ~get_const_func ~get_const_global) Stack.empty e
    in
    match stack with
    | [] -> Error (`Type_mismatch "const expr returning zero values")
    | _ :: _ :: _ ->
      Error (`Type_mismatch "const expr returning more than one value")
    | [ result ] -> Ok result

  let ref_expr ctx ~get_const_func ~get_const_global (e : Binary.expr) :
    Value.t Value.Ref.t Result.t =
    match expr ctx ~get_const_func ~get_const_global e with
    | Ok (Ref v) -> Ok v
    | Ok _ -> Error `Constant_expression_required
    | Error _ as e -> e
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

  let simple_instruction ctx ~get_const_func ~get_const_global stack = function
    | Binary.I32 i -> Result.ok (i32_instr ctx stack i)
    | Binary.I64 i -> Result.ok (i64_instr ctx stack i)
    | F32 (Const f) ->
      Result.ok @@ Stack.push_f32 stack (Value.F32.of_float32 ctx f)
    | F64 (Const f) ->
      Result.ok
      @@ Stack.push_f64 stack (Value.F64.of_float ctx (Float64.to_float f))
    | V128 (Const v) ->
      Result.ok @@ Stack.push_v128 stack (Value.V128.of_concrete ctx v)
    | Ref (Null t) -> Result.ok @@ Stack.push_ref stack (Value.Ref.null ctx t)
    | Ref (Func id) ->
      let* f = get_const_func id in
      let value = Value.Ref (Func (Some f)) in
      Result.ok @@ Stack.push stack value
    | Global (Get id) ->
      let* g = get_const_global id in
      Result.ok @@ Stack.push stack g
    | _ -> assert false

  let instr ctx ~get_const_func ~get_const_global stack instr =
    match instr.Annotated.raw with
    | Binary.Simple i ->
      simple_instruction ctx ~get_const_func ~get_const_global stack i
    | _ -> assert false

  (* TODO: the modul parameter can probably be removed *)
  let expr ctx ~get_const_func ~get_const_global (e : Binary.expr) :
    Value.t Result.t =
    let* stack =
      list_fold_left (instr ctx ~get_const_func ~get_const_global) Stack.empty e
    in
    match stack with
    | [] -> Error (`Type_mismatch "const expr returning zero values")
    | _ :: _ :: _ ->
      Error (`Type_mismatch "const expr returning more than one value")
    | [ result ] -> Ok result

  let ref_expr ctx ~get_const_func ~get_const_global (e : Binary.expr) :
    Value.t Value.Ref.t Result.t =
    match expr ctx ~get_const_func ~get_const_global e with
    | Ok (Ref v) -> Ok v
    | Ok _ -> Error `Constant_expression_required
    | Error _ as e -> e
end
