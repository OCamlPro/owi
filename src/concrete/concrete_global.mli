(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

(** runtime global *)

type t =
  { mutable value : Concrete_value.t
  ; label : string option
  ; mut : Binary.mut
  ; typ : Binary.val_type
  }

val value : t -> Concrete_value.t

val set_value : t -> Concrete_value.t -> unit

val typ : t -> Binary.val_type

val mut : t -> Binary.mut

val backup : t -> t

val recover : from_:t -> to_:t -> unit
