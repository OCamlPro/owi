(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

type t = Abstract_domain.binary

let size = Units.In_bits.of_int 32

let unknown ctx = Abstract_domain.binary_unknown ~size ctx

let of_float ctx _ = unknown ctx

let to_binary f = f

let eq ctx f1 f2 = Abstract_domain.Binary_Forward.beq ~size ctx f1 f2

let reinterpret_i64 ctx _i = unknown ctx
