(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax

let run_file exec filename =
  let* script = Parse.Text.Script.from_file filename in
  exec script

let cmd_concrete ~files ~no_exhaustion =
  let exec = Script.exec ~no_exhaustion in
  list_iter (run_file exec) files

let cmd_symbolic ~files ~no_exhaustion =
  let exec = Script_symbolic.exec ~no_exhaustion in
  list_iter (run_file exec) files

let cmd_abstract ~files ~no_exhaustion ~debug_trace =
  begin match debug_trace with
  | Some path -> Abstract_trace.enable path
  | None -> ()
  end;
  let exec = Script_abstract.exec ~no_exhaustion in
  let result = list_iter (run_file exec) files in
  Abstract_trace.close ();
  result
