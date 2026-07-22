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

  type t = extern_func Link_env.t

  val get_memory : t -> int -> memory choice

  val get_func : t -> int -> Kind.func

  val get_table : t -> int -> table choice

  val get_elem : t -> int -> elem

  val get_data : t -> int -> data choice

  val get_global : t -> int -> global choice

  val get_extern_func : t -> int -> extern_func

  val get_types : t -> Binary.sub_type array

  val get_type_groups : t -> (int * int) array
end
