(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

type !+'a t

type key

val empty : 'a t

val find_opt : key -> 'a t -> 'a option

val add : 'a -> 'a t -> 'a t * key

val add_manual : key -> 'a -> 'a t -> 'a t

val next_key : 'a t -> key

val succ_key : key -> key

val plus_key : key -> int -> key

val unsafe_to_int : key -> int

val unsafe_of_int : int -> key

val pp : 'a Fmt.t -> 'a t Fmt.t

val pp_key : key Fmt.t
