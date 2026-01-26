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

type boolean = Smtml.Typed.Bool.t

type i32 = Smtml.Typed.Bitv32.t

type i64 = Smtml.Typed.Bitv64.t

type f32 = Smtml.Typed.Float32.t

type f64 = Smtml.Typed.Float64.t

type v128 = Smtml.Typed.Bitv128.t

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
