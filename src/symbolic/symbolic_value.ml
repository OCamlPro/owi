(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

module Boolean = Symbolic_boolean
module I32 = Symbolic_i32
module F32 = Symbolic_f32
module I64 = Symbolic_i64
module F64 = Symbolic_f64
module V128 = Symbolic_v128
module Ref = Symbolic_ref

type boolean = Symbolic_boolean.t

type i32 = I32.t

type i64 = I64.t

type f32 = F32.t

type f64 = F64.t

type v128 = V128.t

type t =
  | I32 of I32.t
  | I64 of I64.t
  | F32 of F32.t
  | F64 of F64.t
  | V128 of V128.t
  | Ref of Ref.t

let pp fmt = function
  | I32 i -> I32.pp fmt i
  | I64 i -> I64.pp fmt i
  | F32 f -> F32.pp fmt f
  | F64 f -> F64.pp fmt f
  | V128 e -> V128.pp fmt e
  | Ref r -> Ref.pp fmt r
