(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type 'a t

val get_at : 'a t -> int -> 'a option

val get_by_name : 'a t -> string -> int option

val create : 'a Dynarray.t -> (string, int) Hashtbl.t -> 'a t

val fold : (int -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

val map : ('a -> 'b) -> 'a t -> 'b t

val monadic_map : ('a -> 'b Result.t) -> 'a t -> 'b t Result.t

val to_array : 'a t -> 'a array

val pp : 'a Fmt.t -> Format.formatter -> 'a t -> unit
