(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

module type T = sig
  type t

  type memory

  type data

  type global

  type elem

  type table

  type extern_func

  type 'a choice

  val get_memory : modul:int -> t -> int -> memory choice

  val get_func : modul:int -> t -> int -> Kind.func

  val get_table : modul:int -> t -> int -> table choice

  val get_elem : modul:int -> t -> int -> elem

  val get_data : modul:int -> t -> int -> data

  val get_global : modul:int -> t -> int -> global choice

  val get_extern_func : modul:int -> t -> int -> extern_func

  val get_init_code : modul:int -> t -> Binary.expr Annotated.t
end
