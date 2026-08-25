(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

type !+'a t

val empty : 'a t

val find_opt : int -> 'a t -> 'a option

val add : 'a -> 'a t -> 'a t * int

val add_manual : int -> 'a -> 'a t -> 'a t

val next_key : 'a t -> int

val pp : 'a Fmt.t -> 'a t Fmt.t
