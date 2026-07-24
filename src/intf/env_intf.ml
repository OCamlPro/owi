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

  type 'a choice

  val get_memory : modul:int -> extern_func Link.State.t -> int -> memory choice

  val get_func : modul:int -> extern_func Link.State.t -> int -> Kind.func

  val get_table : modul:int -> extern_func Link.State.t -> int -> table choice

  val get_elem : modul:int -> extern_func Link.State.t -> int -> elem

  val get_data : modul:int -> extern_func Link.State.t -> int -> data choice

  val get_global : modul:int -> extern_func Link.State.t -> int -> global choice

  val get_extern_func :
    modul:int -> extern_func Link.State.t -> int -> extern_func
end
