(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

val cmd :
  unsafe:bool -> out_file:Fpath.t option -> source_file:Fpath.t -> unit Result.t
