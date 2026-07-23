(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

type t = Abstract_domain.binary

let size = Units.In_bits.of_int 128

let unknown_sized size ctx =
  Abstract_domain.binary_unknown ~size:(Units.In_bits.of_int size) ctx

let unknown ctx = Abstract_domain.binary_unknown ~size ctx

let to_binary v = v

let of_concrete ctx _i = unknown ctx

let eq ctx _v1 _v2 = Abstract_domain.boolean_unknown ctx

let to_i32x4 ctx _v =
  ( unknown_sized 32 ctx
  , unknown_sized 32 ctx
  , unknown_sized 32 ctx
  , unknown_sized 32 ctx )

let to_i64x2 ctx _v = (unknown_sized 64 ctx, unknown_sized 64 ctx)
