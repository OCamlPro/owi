(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

exception RecursiveFunctionCall

val eval_exprs :
     env:Env.Abstract.t
  -> modul:Env.Abstract.modul
  -> Abstract_state.t
  -> Abstract_state.t

val modul : env:Env.Abstract.t -> modul:Env.Abstract.modul -> Abstract_state.t

val modul_with_ctx :
     env:Env.Abstract.t
  -> modul:Env.Abstract.modul
  -> Abstract_domain.Context.t
  -> Abstract_state.t

val exec_vfunc_from_outside :
     env:Env.Abstract.t
  -> ctx:Abstract_domain.Context.t
  -> locals:Abstract_value.t Abstract_locals.t
  -> Abstract_extern.Func.t Kind.func
  -> Abstract_state.t Result.t
