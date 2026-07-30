(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

module type T = sig
  type value

  type context

  val empty_context : unit -> context

  val value_of_concrete : context -> Concrete_value.t -> value

  type memory

  val init_memory : Binary.Mem.Type.limits -> memory

  val get_memory_limits : memory -> Binary.Mem.Type.limits

  type table

  val init_table : ?label:string -> Binary.Table.Type.t -> table

  val get_table_size : table -> int

  (* TODO: could be stored at link time instead *)
  val get_table_type : table -> Binary.Table.Type.t

  type elem

  val elem_of_concrete_ref_list : Concrete_ref.t list -> elem

  type extern_func

  val to_func_type : extern_func -> Binary.func_type
end
