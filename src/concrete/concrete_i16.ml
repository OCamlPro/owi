(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

type t = int

let of_i8x2 a b = a land 0xFF lor ((b land 0xFF) lsl 8)

let wrap x = x land 0xFFFF

let add x y = wrap (x + y)

let sub x y = wrap (x - y)
