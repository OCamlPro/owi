(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

module Size = struct
  let b32 = Units.In_bits.s32

  let b64 = Units.In_bits.of_int 64

  let b128 = Units.In_bits.of_int 128

  let equal s1 s2 = Units.In_bits.compare s1 s2 = 0
end

type boolean = Abstract_boolean.t

type i32 = Abstract_i32.t

type i64 = Abstract_i64.t

type f32 = Abstract_f32.t

type f64 = Abstract_f64.t

type v128 = Abstract_v128.t

module Boolean = Abstract_boolean
module I32 = Abstract_i32
module I64 = Abstract_i64
module F32 = Abstract_f32
module F64 = Abstract_f64
module V128 = Abstract_v128
module Ref = Abstract_ref

type t =
  | I32 of i32
  | I64 of i64
  | F32 of f32
  | F64 of f64
  | V128 of v128
  | Ref of Ref.t

let pp ppf = function
  | I32 _b -> Fmt.pf ppf "i32 ..."
  | I64 _b -> Fmt.pf ppf "i64 ..."
  | F32 _b -> Fmt.pf ppf "f32 ..."
  | F64 _b -> Fmt.pf ppf "f64 ..."
  | V128 _v -> Fmt.pf ppf "v128 ..."
  | Ref _r -> Fmt.pf ppf "ref ..."

let pp_with_ctx ctx ppf = function
  | I32 b -> Fmt.pf ppf "i32 %a" (Abstract_i32.pp ctx) b
  | I64 b -> Fmt.pf ppf "i64 %a" (Abstract_i64.pp ctx) b
  | F32 _b -> Fmt.pf ppf "f32 ..."
  | F64 _b -> Fmt.pf ppf "f64 ..."
  | V128 _v -> Fmt.pf ppf "v128 ..."
  | Ref _r -> Fmt.pf ppf "ref ..."

let to_binary = function
  | I32 b -> Abstract_i32.to_binary b
  | I64 b -> Abstract_i64.to_binary b
  | F32 b -> Abstract_f32.to_binary b
  | F64 b -> Abstract_f64.to_binary b
  | V128 v -> Abstract_v128.to_binary v
  | Ref _r -> assert false

let of_binary size x =
  match Units.In_bits.to_int size with
  | 32 -> I32 (I32.of_binary x)
  | 64 -> I64 (I64.of_binary x)
  | _ -> assert false

let size_of = function
  | I32 _ | F32 _ -> Size.b32
  | I64 _ | F64 _ -> Size.b64
  | V128 _ -> Size.b128
  | Ref _ -> assert false

let to_boolean ctx x =
  let size = Size.b32 in
  let zero = Abstract_domain.Binary_Forward.biconst ~size Z.zero ctx in
  Abstract_domain.Binary_Forward.beq ~size ctx (to_binary x) zero

let top size ctx = of_binary size @@ Abstract_domain.binary_empty ~size ctx

let of_script_const ctx ~ty:_ : Wast.const -> t = function
  | Wast.Const_I32 v -> I32 (I32.of_int32 ctx v)
  | Wast.Const_I64 v -> I64 (I64.of_int64 ctx v)
  | Wast.Const_F32 v -> F32 (F32.of_float32 ctx v)
  | Wast.Const_F64 v -> F64 (F64.of_float ctx v)
  | Wast.Const_V128 v -> V128 (V128.of_concrete ctx v)
  | Const_null (Some (Func_ht | NoFunc_ht | TypeUse _)) -> Ref (Func None)
  | Const_null (Some (Extern_ht | NoExtern_ht)) -> Ref (Extern None)
  | Const_null (Some (Any_ht | None_ht)) -> Ref NullRef
  | Const_null (Some (Exn_ht | NoExn_ht)) -> Ref NullExn
  | _ -> assert false

let of_concrete ctx : Concrete_value.t -> t = function
  | I32 v -> I32 (I32.of_int32 ctx v)
  | I64 v -> I64 (I64.of_int64 ctx v)
  | F32 v -> F32 (F32.of_float32 ctx v)
  | F64 v -> F64 (F64.of_float ctx (Concrete_f64.to_float v))
  | V128 v -> V128 (V128.of_concrete ctx v)
  | _ -> assert false

let equal_script_result ctx =
  let compare_f32 (script_result : Wast.result_f32) v =
    match script_result with
    | Concrete f ->
      Boolean.can_be_true ctx @@ F32.eq ctx (F32.of_float32 ctx f) v
    | Nan_canon -> true
    | Nan_arith -> true
  in
  let compare_f64 (script_result : Wast.result_f64) v =
    match script_result with
    | Concrete f ->
      Boolean.can_be_true ctx
      @@ F64.eq ctx (F64.of_float ctx (Concrete_f64.to_float f)) v
    | Nan_canon -> true
    | Nan_arith -> true
  in
  let compare_v128 (script_result : Wast.result_v128) (const : V128.t) =
    match script_result with
    | Concrete v ->
      Boolean.can_be_true ctx @@ V128.eq ctx (V128.of_concrete ctx v) const
    | F32x4 (a, b, c, d) ->
      let a', b', c', d' = V128.to_i32x4 ctx const in
      let a', b', c', d' =
        ( F32.reinterpret_i32 ctx a'
        , F32.reinterpret_i32 ctx b'
        , F32.reinterpret_i32 ctx c'
        , F32.reinterpret_i32 ctx d' )
      in
      compare_f32 a a' && compare_f32 b b' && compare_f32 c c'
      && compare_f32 d d'
    | F64x2 (a, b) ->
      let a', b' = V128.to_i64x2 ctx const in
      let a', b' = (F64.reinterpret_i64 ctx a', F64.reinterpret_i64 ctx b') in
      compare_f64 a a' && compare_f64 b b'
  in
  fun ~ty:_ script_result v ->
    match (script_result, v) with
    | Wast.Result_I32 n, I32 n' ->
      Boolean.can_be_true ctx @@ I32.eq ctx (Abstract_i32.of_int32 ctx n) n'
    | Result_I64 n, I64 n' ->
      Boolean.can_be_true ctx @@ I64.eq ctx (Abstract_i64.of_int64 ctx n) n'
    | Result_F32 script_result, F32 v -> compare_f32 script_result v
    | Result_F64 script_result, F64 v -> compare_f64 script_result v
    | Result_V128 script_result, V128 v -> compare_v128 script_result v
    (* | Result_V128 script_result, V128 v -> compare_v128 script_result v *)
    (* | Result_null None, Ref (NullRef | NullExn | Func None | Extern None) -> *)
    (* | Result_null (Some (NoFunc_ht | Func_ht)), Ref (Func None) -> true *)
    (* | Result_null (Some (Extern_ht | NoExtern_ht)), Ref (Extern None) -> true *)
    (* | Result_null (Some (Exn_ht | NoExn_ht)), Ref NullExn -> true *)
    (* | Result_null (Some (Any_ht | None_ht)), Ref NullRef -> true *)
    (* | Result_null _, Ref _ -> true *)
    (* | Result_extern n, Ref (Extern (Some ref)) -> *)
    (*   begin match Ref.Extern.cast ref ty with *)
    (*   | None -> false *)
    (*   | Some n' -> n = n' *)
    (*   end *)
    (* | Result_func_ref, Ref (Func _) -> *)
    (*   (* TODO: FIX! This is probably unsound! *) *)
    (*   true *)
    | ( ( Result_I32 _ | Result_I64 _ | Result_F32 _ | Result_F64 _
        | Result_V128 _ | Result_null _ | Result_host _ )
      , _ ) ->
      false
    | _, _ -> assert false
