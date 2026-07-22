(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

(** Module to execute a full Wasm script. *)

(** execute a Wasm script *)
val exec : no_exhaustion:bool -> Wast.script -> unit Result.t
