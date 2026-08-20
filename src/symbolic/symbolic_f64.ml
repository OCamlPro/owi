(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

include Smtml.Typed.Float64

let of_bits x = Smtml.Typed.Float64.reinterpret_i64 x

let to_bits x = Smtml.Typed.Float64.to_bv x

let pmin _ = assert false

let pmax _ = assert false
