(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

(** Single table *)
type t

include Table_intf.T with type reference := Symbolic_ref.t and type t := t

val of_concrete : Concrete_table.t -> t

module Collection :
  Collection.S with type concrete := Concrete_table.t and type symbolic := t
