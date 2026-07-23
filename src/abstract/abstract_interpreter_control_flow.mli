(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

val eval_exprs :
     Abstract_extern.Func.t Linked.Module.t
  -> Abstract_state.t
  -> Abstract_extern.Func.t Link_env.t Dynarray.t
  -> Abstract_state.t
