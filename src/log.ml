(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021 Léo Andrès *)
(* Copyright © 2021 Pierre Chambart *)

let debug_on = ref false

let profiling_on = ref false

let debug0 t : unit = if !debug_on then Format.pp_err t

let debug1 t a : unit = if !debug_on then Format.pp_err t a

let debug2 t a b : unit = if !debug_on then Format.pp_err t a b

let debug3 t a b c : unit = if !debug_on then Format.pp_err t a b c

let debug4 t a b c d : unit = if !debug_on then Format.pp_err t a b c d

let debug5 t a b c d e : unit = if !debug_on then Format.pp_err t a b c d e

let profile0 t : unit = if !profiling_on then Format.pp_err t

let profile1 t a : unit = if !profiling_on then Format.pp_err t a

let profile2 t a b : unit = if !profiling_on then Format.pp_err t a b

let profile3 t a b c : unit = if !profiling_on then Format.pp_err t a b c

let profile4 t a b c d : unit = if !profiling_on then Format.pp_err t a b c d

let profile5 t a b c d e : unit =
  if !profiling_on then Format.pp_err t a b c d e

let err f = Format.kasprintf failwith f
