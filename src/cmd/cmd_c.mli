(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

val cmd :
     symbolic_parameters:Symbolic_parameters.t
  -> arch:int
  -> property:Fpath.t option
  -> testcomp:bool
  -> opt_lvl:string
  -> includes:Fpath.t list
  -> files:Fpath.t list
  -> eacsl:bool
  -> out_file:Fpath.t option
  -> unit Result.t
