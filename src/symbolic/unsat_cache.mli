(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

type t

val create : unit -> t
(** [create ()] creates an empty cache. *)

val add : t -> Hash_footprint.t -> Smtml.Expr.t list -> unit
(** [add cache fp core] stores an unsat core under the hash footprint [fp]. *)

val lookup : t -> Smtml.Expr.t -> Smtml.Expr.t list option
(** [lookup cache formula] returns an unsat core if the formula is known to be
    unsat via a cached core (currently only exact structural match). *)

val clear : t -> unit
(** [clear cache] removes all entries. *)

val stats : t -> int * int * int
(** [stats cache] returns (number of stored cores, number of lookups). *)

val iter : (Hash_footprint.t -> Smtml.Expr.t list list -> unit) -> t -> unit

val fold : (Hash_footprint.t -> Smtml.Expr.t list list -> 'a -> 'a) -> t -> 'a -> 'a