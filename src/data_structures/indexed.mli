(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type 'a t

val get : 'a t -> 'a

val get_index : 'a t -> int

val map : ('a -> 'b) -> 'a t -> 'b t

val return : int -> 'a -> 'a t

val get_at : int -> 'a t list -> 'a option

val has_index : int -> 'a t -> bool

val to_assoc_list : 'a t list -> (int * 'a) list
