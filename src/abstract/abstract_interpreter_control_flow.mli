(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

val eval_exprs :
     Abstract_extern.Func.t Linked.Module.t
  -> Abstract_state.t
  -> Abstract_extern.Func.t Link_env.t Dynarray.t
  -> Abstract_state.t

val modul :
     Abstract_extern.Func.t Link.State.t
  -> Abstract_extern.Func.t Linked.Module.t
  -> Abstract_state.t

val modul_with_ctx :
     Abstract_domain.Context.t
  -> Abstract_extern.Func.t Link.State.t
  -> Abstract_extern.Func.t Linked.Module.t
  -> Abstract_state.t
