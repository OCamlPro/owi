(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2025 OCamlPro *)
(* Written by the Owi programmers *)

include
  Interpret_intf.P
    with module Value = Concrete_value
     and module Choice = Concrete_choice
     and module Memory = Concrete_memory
     and module Extern_func = Concrete_extern_func
