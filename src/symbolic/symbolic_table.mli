(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

(** Single table *)
type t

include Table_intf.T with type reference := Symbolic_ref.t and type t := t

(** Collection of tables *)
type collection

val init : unit -> collection

val clone : collection -> collection

val get_table : int -> Concrete_table.t -> collection -> int -> t
