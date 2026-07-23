(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

include
  Env_intf.T
    with type memory := Concrete_memory.t
     and type data := Concrete_data.t
     and type global := Concrete_global.t
     and type elem := Concrete_elem.t
     and type table := Concrete_table.t
     and type extern_func := Concrete_extern.Func.t
     and type 'a choice := 'a Concrete_choice.t
