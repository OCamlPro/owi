(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax

(* TODO: rename this... *)
let env () =
  let env =
    let context = Abstract_domain.root_context () in
    Env.Abstract.empty ~context
  in
  Env.Abstract.link_extern_module ~env ~name:"owi" Abstract_wasm_ffi.owi

let cmd ~debug_trace ~entry_point ~source_file ~unsafe =
  let+ modul, env =
    let* env = env () in
    let* modul = Compile.Wasm.File.until_binary ~unsafe source_file in
    let* modul = Cmd_utils.set_entry_point entry_point false modul in
    Compile.Wasm.Binary.until_abstract_link ~unsafe ~name:None env modul
  in
  Option.iter Abstract_trace.enable debug_trace;
  try
    let state = Abstract_interpreter_control_flow.modul ~env ~modul in
    Abstract_checker.check_module ~env ~modul state.invariant;
    Abstract_trace.close ()
  with Abstract_interpreter_control_flow.RecursiveFunctionCall ->
    Log.err (fun m -> m "Too many recursive calls")
