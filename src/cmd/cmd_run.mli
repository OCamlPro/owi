(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

val cmd :
     unsafe:bool
  -> rac:bool
  -> optimize:bool
  -> files:Fpath.t list
  -> unit Result.t
