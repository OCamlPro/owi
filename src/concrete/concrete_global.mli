(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

(** runtime global *)

type t =
  { mutable value : Concrete_value.t
  ; mut : Text.mut
  ; typ : Text.val_type
  }

val value : t -> Concrete_value.t

val set_value : t -> Concrete_value.t -> unit

val backup : t -> t

val recover : from_:t -> to_:t -> unit
