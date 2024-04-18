(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

let debug_on = ref false

let profiling_on = ref false

let debug0 t : unit = if !debug_on then Format.pp_err t

let debug1 t a : unit = if !debug_on then Format.pp_err t a

let debug2 t a b : unit = if !debug_on then Format.pp_err t a b

let debug5 t a b c d e : unit = if !debug_on then Format.pp_err t a b c d e

let profile3 t a b c : unit = if !profiling_on then Format.pp_err t a b c

let err f = Format.kasprintf failwith f
