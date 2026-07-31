(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

module Concrete_runtime_builder :
  Runtime_builder_intf.T
    with type extern_func = Concrete_extern.Func.t
     and type value = Concrete_value.t
     and type data = Concrete_data.t
     and type context = unit
     and type memory = Concrete_memory.t
     and type elem = Concrete_elem.t
     and type table = Concrete_table.t = struct
  type value = Concrete_value.t

  let value_of_concrete () v = v

  type memory = Concrete_memory.t

  let init_memory = Concrete_memory.init

  let get_memory_limits = Concrete_memory.get_limits

  type table = Concrete_table.t

  let init_table = Concrete_table.init

  let get_table_size = Concrete_table.size

  let get_table_type = Concrete_table.get_type

  type elem = Concrete_elem.t

  let elem_of_concrete_ref_list l = { Concrete_elem.value = Array.of_list l }

  type extern_func = Concrete_extern.Func.t

  let to_func_type = Concrete_extern.Func.to_func_type

  type context = unit

  let empty_context () = ()

  type data = Concrete_data.t
end

include New_link.Make (Concrete_runtime_builder)
