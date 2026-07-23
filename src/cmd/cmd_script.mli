(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

val cmd_concrete : files:Fpath.t list -> no_exhaustion:bool -> unit Result.t

val cmd_symbolic : files:Fpath.t list -> no_exhaustion:bool -> unit Result.t

val cmd_abstract : files:Fpath.t list -> no_exhaustion:bool -> unit Result.t
