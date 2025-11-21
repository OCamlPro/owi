(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type 'a t

val get : 'a t -> 'a

val get_index : 'a t -> int

val map : ('a -> 'b) -> 'a t -> 'b t

val monadic_map : ('a -> 'b Result.t) -> 'a t -> 'b t Result.t

val return : int -> 'a -> 'a t

val get_at : int -> 'a t list -> 'a option

val has_index : int -> 'a t -> bool

val list_to_array : 'a t list -> 'a array

val pp : 'a Fmt.t -> Format.formatter -> 'a t -> unit

val pp_list : 'a Fmt.t -> Format.formatter -> 'a t list -> unit
