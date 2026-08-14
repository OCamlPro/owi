(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax

let env () =
  Env.Abstract.link_extern_module ~env:Env.Abstract.empty ~name:"owi"
    Abstract_wasm_ffi.owi

let cmd ~source_file ~entry_point ~unsafe ~debug_trace =
  let runtime = runtime () in

  let* modul = Compile.File.until_binary ~unsafe source_file in
  let* modul = Cmd_utils.set_entry_point entry_point false modul in
  let+ modul, env =
    Compile.Binary.until_abstract_link ~unsafe ~name:None env modul
  in
  if Option.is_some debug_trace then Abstract_trace.enable ();
  try
    let state = Abstract_interpreter_control_flow.modul ~runtime ~modul in
    Abstract_checker.check_module ~runtime ~modul state.invariant;
    match debug_trace with
    | None -> ()
    | Some path -> Abstract_trace.write_json path
  with Abstract_interpreter_control_flow.RecursiveFunctionCall ->
    Log.err (fun m -> m "Recursive function calls are not supported yet")
