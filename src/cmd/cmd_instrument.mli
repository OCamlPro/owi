(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

val cmd :
     debug:bool
  -> unsafe:bool
  -> symbolic:bool
  -> files:Fpath.t list
  -> unit Result.t
