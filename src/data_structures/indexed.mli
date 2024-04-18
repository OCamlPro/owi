(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type 'a t

val get : 'a t -> 'a

val get_index : 'a t -> int

val map : ('a -> 'b) -> 'a t -> 'b t

val return : int -> 'a -> 'a t

val get_at : int -> 'a t list -> 'a option

val get_at_exn : int -> 'a t list -> 'a

val has_index : int -> 'a t -> bool

val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
