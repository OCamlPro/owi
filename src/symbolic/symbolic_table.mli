(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

(** Single table *)
type t = Symbolic_table_collection.table

include
  Table_intf.T
    with type reference := Symbolic_ref.t
     and type t := t
     and type 'a choice := 'a Symbolic_choice.t

val replace : t -> unit Symbolic_choice.t
