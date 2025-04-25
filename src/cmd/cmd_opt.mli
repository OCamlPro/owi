(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

val cmd :
  unsafe:bool -> source_file:Fpath.t -> out_file:Fpath.t option -> unit Result.t
