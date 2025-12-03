(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type i32 = Int32.t

type i64 = Int64.t

type f32 = Float32.t

type f64 = Float64.t

type v128 = Concrete_v128.t

module Boolean = Concrete_boolean
module Ref = Concrete_ref
module I32 = Concrete_i32
module I64 = Concrete_i64
module F32 = Concrete_f32
module F64 = Concrete_f64
module V128 = Concrete_v128

type t =
  | I32 of i32
  | I64 of i64
  | F32 of f32
  | F64 of f64
  | V128 of v128
  | Ref of Ref.t

let pp ppf =
  let open Fmt in
  function
  | I32 i -> pf ppf "i32.const %a" I32.pp i
  | I64 i -> pf ppf "i64.const %a" I64.pp i
  | F32 f -> pf ppf "f32.const %a" F32.pp f
  | F64 f -> pf ppf "f64.const %a" F64.pp f
  | V128 v -> pf ppf "v128.const %a" V128.pp v
  | Ref r -> pf ppf "ref %a" Ref.pp r
