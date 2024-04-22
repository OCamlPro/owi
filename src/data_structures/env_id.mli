(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type t

type 'a collection

val empty : 'a collection

val with_fresh_id :
  (t -> ('a * 'b) Result.t) -> 'a collection -> ('a collection * 'b) Result.t

val get : t -> 'a collection -> 'a

val map : ('a -> 'b) -> 'a collection -> 'b collection

module Map : Map.S with type key = t

module Tbl : Hashtbl.S with type key = t
