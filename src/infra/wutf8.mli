(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

(** Utility functions to work with utf8. *)

val check_utf8 : string -> unit Result.t

val encode : int list -> string
