(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

val cmd :
     symbolic_parameters:Cmd_sym.parameters
  -> arch:int
  -> property:Fpath.t option
  -> testcomp:bool
  -> opt_lvl:string
  -> includes:Fpath.t list
  -> files:Fpath.t list
  -> concolic:bool
  -> eacsl:bool
  -> out_file:Fpath.t option
  -> unit Result.t
