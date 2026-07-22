(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

val check_result : expected:string -> got:'a Result.t -> unit Result.t

val check_error : expected:string -> got:Result.err -> unit Result.t
