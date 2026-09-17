(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

type t

val empty : t

val add_to_model : Concrete_value.t -> t -> t

val get_model : t -> Concrete_value.t list

val model_is_empty : t -> bool
