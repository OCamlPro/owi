(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax

let run_file exec filename =
  let* script = Parse.Text.Script.from_file filename in
  exec script

let cmd ~profiling ~debug ~optimize ~files ~no_exhaustion =
  let exec = Script.exec ~no_exhaustion ~optimize in
  if profiling then Log.profiling_on := true;
  if debug then Log.debug_on := true;
  list_iter (run_file exec) files
