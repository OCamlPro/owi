(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

module Size = struct
  let b32 = Units.In_bits.s32

  let b64 = Units.In_bits.of_int 64

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
  | _ -> .

let pp_with_ctx ctx ppf = function
  | I32 b -> Fmt.pf ppf "i32 %a" (Abstract_i32.pp ctx) b
  | I64 b -> Fmt.pf ppf "i64 %a" (Abstract_i64.pp ctx) b
  | _ -> .

let to_binary = function
  | I32 b -> Abstract_i32.to_binary b
  | I64 b -> Abstract_i64.to_binary b
  | _ -> .

let of_binary size x =
  match Units.In_bits.to_int size with
  | 32 -> I32 (I32.of_binary x)
  | 64 -> I64 (I64.of_binary x)
  | _ -> assert false

let size_of = function I32 _ -> Size.b32 | I64 _ -> Size.b64 | _ -> .

let to_boolean ctx x =
  let size = Size.b32 in
  let zero = Abstract_domain.Binary_Forward.biconst ~size Z.zero ctx in
  Abstract_domain.Binary_Forward.beq ~size ctx (to_binary x) zero

let top size ctx = of_binary size @@ Abstract_domain.binary_empty ~size ctx
