(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

val fuzz :
     arch:int
  -> eacsl:bool
  -> entry_point:string option
  -> files:Fpath.t list
  -> includes:Fpath.t list
  -> opt_lvl:string
  -> out_file:Fpath.t option
  -> property:Fpath.t option
  -> rounds:int option
  -> seed:int option
  -> testcomp:bool
  -> timeout:float option
  -> timeout_instr:int option
  -> unsafe:bool
  -> workspace:Fpath.t
  -> unit Result.t

val hunt :
     arch:int
  -> eacsl:bool
  -> entry_point:string option
  -> files:Fpath.t list
  -> includes:Fpath.t list
  -> opt_lvl:string
  -> out_file:Fpath.t option
  -> property:Fpath.t option
  -> rounds:int option
  -> seed:int option
  -> symbolic_parameters:Symbolic_parameters.t
  -> workspace:Fpath.t
  -> unit Result.t

val sym :
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
  -> workspace:Fpath.t
  -> unit Result.t
