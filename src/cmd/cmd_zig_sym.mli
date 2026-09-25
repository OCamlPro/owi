(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

val cmd :
     entry_point:string option
  -> files:Fpath.t list
  -> includes:Fpath.t list
  -> out_file:Fpath.t option
  -> symbolic_parameters:Symbolic_parameters.t
  -> unit Result.t
