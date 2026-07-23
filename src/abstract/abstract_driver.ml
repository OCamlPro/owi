(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

let modul (link_state : Abstract_extern.Func.t Link.State.t)
  (m : Abstract_extern.Func.t Linked.Module.t) =
  let envs = Link.State.get_envs link_state in
  let env = m.env in
  let abs_state = Abstract_state.empty env () in
  Abstract_interpreter_control_flow.eval_exprs m abs_state envs
