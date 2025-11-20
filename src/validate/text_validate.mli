(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

(** Initial check done on a module. *)

(** check a module *)
val modul : Text.Module.t -> Text.Module.t Result.t
