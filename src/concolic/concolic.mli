(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2025 OCamlPro *)
(* Written by the Owi programmers *)

include
  Interpret_intf.P
    with module Value = Concolic_value
     and module Choice = Concolic_choice
     and module Memory = Concolic_memory
     and module Extern_func = Concolic_extern_func
     and type Env.t = Concolic_extern_func.extern_func Link_env.t

val convert_module_to_run :
  Extern_func.extern_func Link.module_to_run -> Module_to_run.t

val backup : Module_to_run.t -> Extern_func.extern_func Link_env.backup

val recover : Extern_func.extern_func Link_env.backup -> Module_to_run.t -> unit
