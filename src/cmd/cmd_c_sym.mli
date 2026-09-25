(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

val cmd :
     arch:int
  -> eacsl:bool
  -> entry_point:string option
  -> files:Fpath.t list
  -> includes:Fpath.t list
  -> opt_lvl:string
  -> out_file:Fpath.t option
  -> property:Fpath.t option
  -> symbolic_parameters:Symbolic_parameters.t
  -> testcomp:bool
  -> unit Result.t
