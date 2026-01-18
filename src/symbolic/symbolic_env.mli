(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2025 OCamlPro *)
(* Written by the Owi programmers *)

include
  Env_intf.T
    with type memory := Symbolic_memory.t
     and type data := Symbolic_data.t
     and type global := Symbolic_global.t
     and type elem := Symbolic_elem.t
     and type table := Symbolic_table.t
     and type extern_func := Symbolic_extern_func.extern_func
     and type 'a choice := 'a Symbolic_choice.t
