(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

val abs :
     debug_trace:string option
  -> entry_point:string option
  -> files:Fpath.t list
  -> includes:Fpath.t list
  -> opt_lvl:string
  -> out_file:Fpath.t option
  -> unsafe:bool
  -> workspace:Fpath.t
  -> unit Result.t

val fuzz :
     entry_point:string option
  -> files:Fpath.t list
  -> includes:Fpath.t list
  -> opt_lvl:string
  -> out_file:Fpath.t option
  -> rounds:int option
  -> seed:int option
  -> timeout:float option
  -> timeout_instr:int option
  -> unsafe:bool
  -> workspace:Fpath.t
  -> unit Result.t

val hunt :
     entry_point:string option
  -> files:Fpath.t list
  -> includes:Fpath.t list
  -> opt_lvl:string
  -> out_file:Fpath.t option
  -> rounds:int option
  -> seed:int option
  -> symbolic_parameters:Symbolic_parameters.t
  -> workspace:Fpath.t
  -> unit Result.t

val run :
     entry_point:string option
  -> files:Fpath.t list
  -> includes:Fpath.t list
  -> opt_lvl:string
  -> out_file:Fpath.t option
  -> timeout:float option
  -> timeout_instr:int option
  -> unsafe:bool
  -> workspace:Fpath.t
  -> unit Result.t

val sym :
     arch:int
  -> entry_point:string option
  -> files:Fpath.t list
  -> includes:Fpath.t list
  -> opt_lvl:string
  -> out_file:Fpath.t option
  -> symbolic_parameters:Symbolic_parameters.t
  -> workspace:Fpath.t
  -> unit Result.t
