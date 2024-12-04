(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

val cmd :
     profiling:bool
  -> debug:bool
  -> unsafe:bool
  -> optimize:bool
  -> outfile:Fpath.t option
  -> file:Fpath.t
  -> unit Result.t
