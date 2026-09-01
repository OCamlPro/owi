(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

type context = unit

type boolean = Concrete_boolean.t

type i32 = Concrete_i32.t

type i64 = Concrete_i64.t

type f32 = Concrete_f32.t

type f64 = Concrete_f64.t

type v128 = Concrete_v128.t

module Boolean = Concrete_boolean
module I32 = Concrete_i32
module I64 = Concrete_i64
module F32 = Concrete_f32
module F64 = Concrete_f64
module V128 = Concrete_v128
module Ref = Concrete_ref

type reference = t Concrete_ref.t

and t =
  | I32 of i32
  | I64 of i64
  | F32 of f32
  | F64 of f64
  | V128 of v128
  | Ref of reference

let pp ppf =
  let open Fmt in
  function
  | I32 i -> pf ppf "i32.const %a" I32.pp i
  | I64 i -> pf ppf "i64.const %a" I64.pp i
  | F32 f -> pf ppf "f32.const %a" F32.pp f
  | F64 f -> pf ppf "f64.const %a" F64.pp f
  | V128 v -> pf ppf "v128.const %a" V128.pp v
  | Ref r -> pf ppf "ref %a" Ref.pp r

let of_script_const ~ty = function
  | (Const_I32 v : Wast.const) -> (I32 v : t)
  | Const_I64 v -> I64 v
  | Const_F32 v -> F32 v
  | Const_F64 v -> F64 v
  | Const_V128 v -> V128 v
  | Const_extern i -> Ref (Concrete_ref.extern ty i)
  | Const_host i ->
    Ref (Concrete_ref.any_convert_extern (Concrete_ref.extern ty i))
  (* TODO: not ideal, the following are a duplication of Concrete_ref.null
     applying on Text.heap_type instead of Binary.heap_type. *)
  | Const_null (Some (Func_ht | NoFunc_ht | TypeUse _)) -> Ref (Func None)
  | Const_null (Some (Extern_ht | NoExtern_ht)) -> Ref (Extern None)
  | Const_null (Some (Any_ht | None_ht | Struct_ht | Array_ht)) -> Ref NullRef
  | Const_null (Some (Exn_ht | NoExn_ht)) -> Ref NullExn
  | Const_null (Some (Eq_ht | I31_ht)) -> Ref NullI31
  | _ -> assert false

let equal_script_result =
  let compare_f32 (script_result : Wast.result_f32) v =
    match script_result with
    | Concrete f ->
      F32.eq f v || String.equal (F32.to_string f) (F32.to_string v)
    | Nan_canon -> F32.is_pos_nan v || F32.is_neg_nan v
    | Nan_arith ->
      let pos_nan = F32.to_bits F32.pos_nan in
      I32.eq (I32.logand (F32.to_bits v) pos_nan) pos_nan
  in
  let compare_f64 (script_result : Wast.result_f64) v =
    match script_result with
    | Concrete f ->
      F64.eq f v || String.equal (F64.to_string f) (F64.to_string v)
    | Nan_canon -> F64.is_pos_nan v || F64.is_neg_nan v
    | Nan_arith ->
      let pos_nan = F64.to_bits F64.pos_nan in
      I64.eq (I64.logand (F64.to_bits v) pos_nan) pos_nan
  in
  let compare_v128 (script_result : Wast.result_v128) (const : V128.t) =
    match script_result with
    | Concrete v -> V128.eq v const
    | F32x4 (a, b, c, d) ->
      let a', b', c', d' = V128.to_i32x4 const in
      let a', b', c', d' =
        ( F32.reinterpret_i32 a'
        , F32.reinterpret_i32 b'
        , F32.reinterpret_i32 c'
        , F32.reinterpret_i32 d' )
      in
      compare_f32 a a' && compare_f32 b b' && compare_f32 c c'
      && compare_f32 d d'
    | F64x2 (a, b) ->
      let a', b' = V128.to_i64x2 const in
      let a', b' = (F64.reinterpret_i64 a', F64.reinterpret_i64 b') in
      compare_f64 a a' && compare_f64 b b'
  in
  fun ~ty script_result v ->
    match (script_result, v) with
    | Wast.Result_I32 n, I32 n' -> I32.eq n n'
    | Result_I64 n, I64 n' -> I64.eq n n'
    | Result_F32 script_result, F32 v -> compare_f32 script_result v
    | Result_F64 script_result, F64 v -> compare_f64 script_result v
    | Result_V128 script_result, V128 v -> compare_v128 script_result v
    | ( Result_null None
      , Ref (NullRef | NullExn | Func None | Extern None | NullI31) ) ->
      true
    | Result_null (Some (NoFunc_ht | Func_ht)), Ref (Func None) -> true
    | Result_null (Some (Extern_ht | NoExtern_ht)), Ref (Extern None) -> true
    | Result_null (Some (Exn_ht | NoExn_ht)), Ref NullExn -> true
    | Result_null (Some (Any_ht | None_ht | Struct_ht | Array_ht)), Ref NullRef
      ->
      true
    | Result_null (Some (Eq_ht | I31_ht)), Ref NullI31 -> true
    | Result_i31_ref, Ref (I31 _) -> true
    | Result_struct_ref, Ref (Struct _) -> true
    | Result_array_ref, Ref (Array _) -> true
    | Result_eq_ref, Ref (I31 _ | NullI31 | Struct _ | Array _) -> true
    | Result_extern_ref, Ref (Extern _ | AnyAsExtern _) -> true
    | Result_extern n, Ref (Extern (Some ref)) ->
      begin match Ref.Extern.cast ref ty with
      | None -> false
      | Some n' -> n = n'
      end
    | Result_host n, Ref (ExternAsAny (Some ref)) ->
      begin match Ref.Extern.cast ref ty with
      | None -> false
      | Some n' -> n = n'
      end
    | Result_func_ref, Ref (Func _) ->
      (* TODO: FIX! This is probably unsound! *)
      true
    | ( ( Result_I32 _ | Result_I64 _ | Result_F32 _ | Result_F64 _
        | Result_V128 _ | Result_null _ | Result_host _ | Result_i31_ref
        | Result_struct_ref | Result_array_ref | Result_eq_ref
        | Result_extern_ref | Result_struct _ | Result_array _ | Result_func _
        | Result_exn _ )
      , _ ) ->
      false
    | _, _ -> assert false

let of_concrete () x = x
