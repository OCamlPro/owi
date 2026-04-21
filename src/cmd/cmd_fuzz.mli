(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

val cmd :
     rounds:int option
  -> seed:int option
  -> source_file:Fpath.t
  -> timeout:float option
  -> timeout_instr:int option
  -> unsafe:bool
  -> unit Result.t
