(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

val eval_exprs :
  modul:int -> Abstract_state.t -> Link.Abstract.State.t -> Abstract_state.t

val modul : Link.Abstract.State.t -> modul:int -> Abstract_state.t

val modul_with_ctx :
     Abstract_domain.Context.t
  -> Link.Abstract.State.t
  -> modul:int
  -> Abstract_state.t

val exec_vfunc_from_outside :
     ctx:Abstract_domain.Context.t
  -> locals:Abstract_value.t Abstract_locals.t
  -> modul:int
  -> link_state:Link.Abstract.State.t
  -> Kind.func
  -> Abstract_state.t Result.t
