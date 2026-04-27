(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

type t = int

let wrap x = x land 0xFF

let add x y = wrap (x + y)

let sub x y = wrap (x - y)
