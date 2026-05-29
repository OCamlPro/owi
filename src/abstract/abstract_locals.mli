(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

type 'a t

type key = int

val to_list : 'a t -> (key * 'a) list

val of_list : (key * 'a) list -> 'a t

val empty : 'a t

val fold_on_nonequal_union :
     (key -> 'a option -> 'a option -> 'acc -> 'acc)
  -> 'a t
  -> 'a t
  -> 'acc
  -> 'acc

val add : key -> 'a -> 'a t -> 'a t

val find : int -> 'a t -> 'a

val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
