(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

val eval_exprs :
     Binary.instr Annotated.t list Annotated.t list
  -> Abstract_state.t
  -> Abstract_extern_func.extern_func Link_env.t
  -> Abstract_extern_func.extern_func Link_env.t Dynarray.t
  -> Abstract_state.t
