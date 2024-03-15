(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type 'a t =
  { values : 'a Indexed.t list
  ; named : int String_map.t
  }

val empty : 'a t

val fold : (int -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

val map : ('a Indexed.t -> 'b Indexed.t) -> 'a t -> 'b t
