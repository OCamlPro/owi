(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

exception RecursiveFunctionCall

val eval_exprs :
     runtime:Abstract_runtime.t
  -> modul:Abstract_runtime.modul
  -> Abstract_state.t
  -> Abstract_state.t

val modul :
  runtime:Abstract_runtime.t -> modul:Abstract_runtime.modul -> Abstract_state.t

val modul_with_ctx :
     runtime:Abstract_runtime.t
  -> modul:Abstract_runtime.modul
  -> Abstract_domain.Context.t
  -> Abstract_state.t

val exec_vfunc_from_outside :
     runtime:Abstract_runtime.t
  -> ctx:Abstract_domain.Context.t
  -> locals:Abstract_value.t Abstract_locals.t
  -> Kind.func
  -> Abstract_state.t Result.t
