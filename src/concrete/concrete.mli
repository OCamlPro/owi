(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

include
  Interpret_intf.P
    with type Env.t = Concrete_value.Func.extern_func Link_env.t
     and type Module_to_run.t =
      Concrete_value.Func.extern_func Link.module_to_run
     and type 'a Choice.t = 'a
     and module Value = V
