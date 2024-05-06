(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

val cmd :
     bool
  -> int
  -> string option
  -> bool
  -> string
  -> int
  -> string
  -> Fpath.t list
  -> Fpath.t list
  -> bool
  -> bool
  -> bool
  -> bool
  -> bool
  -> bool
  -> bool
  -> unit Result.t
