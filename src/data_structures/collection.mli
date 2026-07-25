(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

type 'a t

val empty : 'a t

val find : 'a t -> module_id:int -> id:int -> 'a option

val replace : 'a t -> module_id:int -> id:int -> 'a -> 'a t
