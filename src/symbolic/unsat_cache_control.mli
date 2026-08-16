(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

val enable : unit -> unit
val disable : unit -> unit
val get : unit -> Unsat_cache.t option