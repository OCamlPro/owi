(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type 'a t = private
  { values : 'a Indexed.t list
  ; named : int String_map.t
  }

val empty : 'a t

val create : 'a Indexed.t list -> int String_map.t -> 'a t

val fold : (int -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

val map : ('a Indexed.t -> 'b Indexed.t) -> 'a t -> 'b t

val to_array : 'a t -> 'a array

val pp : 'a Fmt.t -> Format.formatter -> 'a t -> unit
