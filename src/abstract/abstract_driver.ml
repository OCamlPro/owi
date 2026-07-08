(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

open Abstract_interpreter_control_flow

let expr (link_state : Abstract_extern_func.extern_func Link.State.t)
  (m : Abstract_extern_func.extern_func Linked.Module.t) =
  let envs = Link.State.get_envs link_state in
  let env = m.env in
  let abs_state = Abstract_state.empty env () in
  eval_exprs m.to_run abs_state env envs
