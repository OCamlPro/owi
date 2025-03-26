(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

(** Module to typecheck a simplified module. *)

(** typecheck a given module *)
val modul : Binary.Module.t -> unit Result.t
