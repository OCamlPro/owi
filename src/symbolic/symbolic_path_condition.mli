(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type t

val empty : t

val add : t -> Symbolic_value.bool -> t

(* TODO: this is needed because *)
val to_list : t -> Symbolic_value.bool list
