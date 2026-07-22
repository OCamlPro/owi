(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

type boolean = Smtml.Typed.Bool.t

type i32 = Smtml.Typed.Bitv32.t

type i64 = Smtml.Typed.Bitv64.t

type f32 = Smtml.Typed.Float32.t

type f64 = Smtml.Typed.Float64.t

type v128 = Smtml.Typed.Bitv128.t

module Boolean = Symbolic_boolean
module I32 = Symbolic_i32
module F32 = Symbolic_f32
module I64 = Symbolic_i64
module F64 = Symbolic_f64
module V128 = Symbolic_v128
module Ref = Symbolic_ref

type t =
  | I32 of Smtml.Typed.Bitv32.t
  | I64 of Smtml.Typed.Bitv64.t
  | F32 of Smtml.Typed.Float32.t
  | F64 of Smtml.Typed.Float64.t
  | V128 of Smtml.Typed.Bitv128.t
  | Ref of Ref.t

let pp fmt = function
  | I32 i -> Smtml.Typed.Bitv32.pp fmt i
  | I64 i -> Smtml.Typed.Bitv64.pp fmt i
  | F32 f -> Smtml.Typed.Float32.pp fmt f
  | F64 f -> Smtml.Typed.Float64.pp fmt f
  | V128 e -> Smtml.Typed.Bitv128.pp fmt e
  | Ref r -> Ref.pp fmt r

let of_script_const ~ty : Wast.const -> t = function
  | Const_I32 v -> I32 (Symbolic_i32.of_int32 v)
  | Const_I64 v -> I64 (Symbolic_i64.of_int64 v)
  | Const_F32 v -> F32 (Symbolic_f32.of_float32 v)
  | Const_F64 v -> F64 (Symbolic_f64.of_float (Float64.to_float v))
  | Const_V128 v -> V128 (Symbolic_v128.of_concrete v)
  | Const_extern i -> Ref (Symbolic_ref.extern ty i)
  (* TODO: not ideal, the following are a duplication of Concrete_ref.null
     applying on Text.heap_type instead of Binary.heap_type. *)
  | Const_null (Some (Func_ht | NoFunc_ht | TypeUse _)) -> Ref (Func None)
  | Const_null (Some (Extern_ht | NoExtern_ht)) -> Ref (Extern None)
  | Const_null (Some (Any_ht | None_ht)) -> Ref NullRef
  | Const_null (Some (Exn_ht | NoExn_ht)) -> Ref NullExn
  | _ -> assert false

let equal_script_result = fun ~ty:_ -> assert false
(*
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
    | Result_null None, Ref (NullRef | NullExn | Func None | Extern None) ->
      true
    | Result_null (Some (NoFunc_ht | Func_ht)), Ref (Func None) -> true
    | Result_null (Some (Extern_ht | NoExtern_ht)), Ref (Extern None) -> true
    | Result_null (Some (Exn_ht | NoExn_ht)), Ref NullExn -> true
    | Result_null (Some (Any_ht | None_ht)), Ref NullRef -> true
    | Result_extern n, Ref (Extern (Some ref)) ->
      begin match Ref.Extern.cast ref ty with
      | None -> false
      | Some n' -> n = n'
      end
    | Result_func_ref, Ref (Func _) ->
      (* TODO: FIX! This is probably unsound! *)
      true
    | ( ( Result_I32 _ | Result_I64 _ | Result_F32 _ | Result_F64 _
        | Result_V128 _ | Result_null _ | Result_host _ )
      , _ ) ->
      false
    | _, _ -> assert false
    *)
