(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

val eval_exprs :
     Abstract_extern.Func.t Link.Linked_module.t
  -> Abstract_state.t
  -> Abstract_extern.Func.t Link.Linked_module.t Dynarray.t
  -> Abstract_state.t

val modul :
     Abstract_extern.Func.t Link.State.t
  -> Abstract_extern.Func.t Link.Linked_module.t
  -> Abstract_state.t

val modul_with_ctx :
     Abstract_domain.Context.t
  -> Abstract_extern.Func.t Link.State.t
  -> Abstract_extern.Func.t Link.Linked_module.t
  -> Abstract_state.t

val exec_vfunc_from_outside :
     ctx:Abstract_domain.Context.t
  -> locals:Abstract_value.t Abstract_locals.t
  -> modul:int
  -> modules:Abstract_extern.Func.t Link.Linked_module.t Dynarray.t
  -> Kind.func
  -> Abstract_state.t Result.t
