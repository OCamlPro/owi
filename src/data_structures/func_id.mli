(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type t

type 'a collection

val empty : 'a collection

val add : 'a -> Binary.func_type -> 'a collection -> t * 'a collection

val get : t -> 'a collection -> 'a option

val get_typ : t -> 'a collection -> Binary.func_type
