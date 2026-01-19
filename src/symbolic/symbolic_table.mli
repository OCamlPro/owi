(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

(** Single table *)
type t = Symbolic_table0.t

include
  Table_intf.T
    with type reference := Symbolic_ref.t
     and type t := t
     and type 'a choice := 'a Symbolic_choice.t

val replace : t -> unit Symbolic_choice.t

val of_concrete : env_id:int -> id:int -> Concrete_table.t -> t
