(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

val cmd :
     unsafe:bool
  -> coverage_criteria:Label.Coverage_criteria.t
  -> source_file:Fpath.t
  -> unit Result.t
