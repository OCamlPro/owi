(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

module Make (Prio : sig
  type t

  val compare : t -> t -> int
end) : sig
  type 'a t

  val empty : unit -> 'a t

  val is_empty : 'a t -> bool

  val pop : 'a t -> 'a option

  val push : Prio.t * 'a -> 'a t -> unit
end
