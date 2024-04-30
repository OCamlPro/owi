(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

(** runtime global *)

open Types

type t =
  { mutable value : Concrete_value.t
  ; label : string option
  ; mut : mut
  ; typ : binary val_type
  }

val value : t -> Concrete_value.t

val set_value : t -> Concrete_value.t -> unit

val typ : t -> binary val_type

val mut : t -> mut
