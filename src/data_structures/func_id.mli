(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Types

type t

type 'a collection

val empty : 'a collection

val add : 'a -> binary func_type -> 'a collection -> t * 'a collection

val get : t -> 'a collection -> 'a option

val get_typ : t -> 'a collection -> binary func_type
