(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

let i32 =
  [| (************************* limit cases and magic *************)
     0l
   ; 1l
   ; 17l
   ; 42l
   ; 666l
   ; 123456l
   ; Int32.min_int
   ; Int32.max_int
   ; (************************* 2^n *******************************)
     2l
   ; 4l
   ; 8l
   ; 16l
   ; 32l
   ; 64l
   ; 128l
   ; 256l
   ; 512l
   ; 1024l
   ; 2048l
   ; 4096l
   ; 8192l
   ; 16384l
   ; 32768l
   ; 65536l
   ; 131072l
   ; 262144l
   ; 524288l
   ; 1048576l
   ; 2097152l
   ; 4194304l
   ; 8388608l
   ; 16777216l
   ; 33554432l
   ; 67108864l
   ; 134217728l
   ; 268435456l
   ; 536870912l
   ; 1073741824l
   ; 2147483648l
  |]

let i64 =
  [| (************************* limit cases and magic *************)
     0L
   ; 1L
   ; 17L
   ; 42L
   ; 666L
   ; 123456L
   ; Int64.min_int
   ; Int64.max_int
   ; (************************* 2^n *******************************)
     2L
   ; 4L
   ; 8L
   ; 16L
   ; 32L
   ; 64L
   ; 128L
   ; 256L
   ; 512L
   ; 1024L
   ; 2048L
   ; 4096L
   ; 8192L
   ; 16384L
   ; 32768L
   ; 65536L
   ; 131072L
   ; 262144L
   ; 524288L
   ; 1048576L
   ; 2097152L
   ; 4194304L
   ; 8388608L
   ; 16777216L
   ; 33554432L
   ; 67108864L
   ; 134217728L
   ; 268435456L
   ; 536870912L
   ; 1073741824L
   ; 2147483648L
   ; 4294967296L
   ; 8589934592L
   ; 17179869184L
   ; 34359738368L
   ; 68719476736L
   ; 137438953472L
   ; 274877906944L
   ; 549755813888L
   ; 1099511627776L
   ; 2199023255552L
   ; 4398046511104L
   ; 8796093022208L
   ; 17592186044416L
   ; 35184372088832L
   ; 70368744177664L
   ; 140737488355328L
   ; 281474976710656L
   ; 562949953421312L
   ; 1125899906842624L
   ; 2251799813685248L
   ; 4503599627370496L
   ; 9007199254740992L
   ; 18014398509481984L
   ; 36028797018963968L
   ; 72057594037927936L
   ; 144115188075855872L
   ; 288230376151711744L
   ; 576460752303423488L
   ; 1152921504606846976L
   ; 2305843009213693952L
   ; 4611686018427387904L
  |]

let f32 =
  (* TODO: avoid going through 64bits floats *)
  Array.map Concrete_f32.of_float
    [| (************************* limit cases and magic ***********)
       0.
     ; -0.
     ; Float.one
     ; Float.infinity
     ; Float.nan
     ; Float.signaling_nan
     ; Float.quiet_nan
     ; Float.pi
     ; Float.max_float
     ; Float.min_float
     ; Float.epsilon
    |]

let f64 =
  Array.map Concrete_f64.of_float
    [| (************************* limit cases and magic ***********)
       0.
     ; -0.
     ; Float.one
     ; Float.infinity
     ; Float.nan
     ; Float.signaling_nan
     ; Float.quiet_nan
     ; Float.pi
     ; Float.max_float
     ; Float.min_float
     ; Float.epsilon
    |]

module Collect = struct
  type t =
    { i32 : Concrete_i32.t list
    ; i64 : Concrete_i64.t list
    ; f32 : Concrete_f32.t list
    ; f64 : Concrete_f64.t list
    }

  let empty = { i32 = []; i64 = []; f32 = []; f64 = [] }

  let rec from_instr collect instr =
    match instr.Annotated.raw with
    | Binary.I32 (Const n) -> { collect with i32 = n :: collect.i32 }
    | I64 (Const n) -> { collect with i64 = n :: collect.i64 }
    | F32 (Const n) -> { collect with f32 = n :: collect.f32 }
    | F64 (Const n) -> { collect with f64 = n :: collect.f64 }
    | Block (_, _, e) | Loop (_, _, e) -> from_expr collect e
    | If_else (_, _, e1, e2) ->
      let collect = from_expr collect e1 in
      from_expr collect e2
    | I32 _ | I64 _ | F32 _ | F64 _ | V128 _ | I8x16 _ | I16x8 _ | I32x4 _
    | I64x2 _ | Ref _ | Local _ | Global _ | Table _ | Elem _ | Memory _
    | Data _ | Drop | Select _ | Nop | Unreachable | Br _ | Br_if _ | Br_table _
    | Br_on_null _ | Br_on_non_null _ | Return | Return_call _
    | Return_call_ref _ | Return_call_indirect _ | Call _ | Call_indirect _
    | Call_ref _ | Any_convert_extern | Extern_convert_any | F32x4 _ | F64x2 _
    | I31 _ | Struct _ | Array _
    | Br_on_cast (_, _, _)
    | Br_on_cast_fail (_, _, _) ->
      collect

  and from_expr collect expr =
    List.fold_left from_instr collect expr.Annotated.raw

  let from_global _globals _collect = raise @@ Failure "TODO"

  let from_table _tables _collect = raise @@ Failure "TODO"

  let from_mem _memories _collect = raise @@ Failure "TODO"

  let from_func funcs collect =
    Array.fold_left
      (fun collect -> function
        | Origin.Imported _ -> raise @@ Failure "TODO"
        | Local { Binary.Func.body; type_f = _; locals = _; id = _ } ->
          from_expr collect body )
      collect funcs

  let from_elem _elem _collect = raise @@ Failure "TODO"

  let from_data _data _collect = raise @@ Failure "TODO"

  let from_module
    { Binary.Module.id = _
    ; types = _
    ; global
    ; table
    ; mem
    ; func
    ; tag = _
    ; elem
    ; data
    ; exports = _
    ; start = _
    ; custom = _
    } =
    empty |> from_global global |> from_table table |> from_mem mem
    |> from_func func |> from_elem elem |> from_data data
end
