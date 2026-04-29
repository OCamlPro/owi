(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

val cmd :
     symbolic_parameters:Symbolic_parameters.t
  -> includes:Fpath.t list
  -> files:Fpath.t list
  -> out_file:Fpath.t option
  -> unit Result.t
