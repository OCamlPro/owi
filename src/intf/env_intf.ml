(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

module type T = sig
  type memory

  type data

  type global

  type elem

  type table

  type extern_func

  type link_state

  type 'a choice

  val get_memory : modul:int -> link_state -> int -> memory choice

  val get_func : modul:int -> link_state -> int -> Kind.func

  val get_table : modul:int -> link_state -> int -> table choice

  val get_elem : modul:int -> link_state -> int -> elem

  val get_data : modul:int -> link_state -> int -> data choice

  val get_global : modul:int -> link_state -> int -> global choice

  val get_extern_func : modul:int -> link_state -> int -> extern_func

  val get_init_code : modul:int -> link_state -> Binary.expr Annotated.t
end
