(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

module Symbolic_runtime_builder : Runtime_builder_intf.T = struct
  type value = Symbolic_value.t

  let value_of_concrete () v = Symbolic_value.of_concrete v

  type memory = Symbolic_memory.t

  let init_memory _ = assert false

  let get_memory_limits _ = assert false

  type table = Symbolic_table.t

  let init_table ?label:_ = assert false

  let get_table_size _ = assert false

  let get_table_type _ = assert false

  type elem = Symbolic_elem.t

  let elem_of_concrete_ref_list _ = assert false

  type extern_func = Symbolic_extern.Func.t

  let to_func_type = Symbolic_extern.Func.to_func_type

  type context = unit

  let empty_context () = ()

  type data = Symbolic_data.t

  let data_of_string = Symbolic_data.of_string
end

include New_link.Make (Symbolic_runtime_builder)
