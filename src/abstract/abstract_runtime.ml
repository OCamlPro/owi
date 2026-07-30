(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

module Abstract_runtime_builder :
  Runtime_builder_intf.T
    with type extern_func = Abstract_extern.Func.t
     and type value = Abstract_value.t = struct
  type value = Abstract_value.t

  let value_of_concrete = Abstract_value.of_concrete

  type memory = Abstract_memory.t

  let init_memory _ = assert false

  let get_memory_limits _ = assert false

  type table = |

  let init_table ?label:_ = assert false

  let get_table_size _ = assert false

  let get_table_type _ = assert false

  type elem = |

  let elem_of_concrete_ref_list _ = assert false

  type extern_func = Abstract_extern.Func.t

  let to_func_type = Abstract_extern.Func.to_func_type

  type context = Abstract_domain.Context.t

  let empty_context = Abstract_domain.root_context
end

include New_link.Make (Abstract_runtime_builder)
