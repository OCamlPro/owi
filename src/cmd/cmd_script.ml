(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021 Léo Andrès *)
(* Copyright © 2021 Pierre Chambart *)

open Owi
open Owi.Syntax

let run_file exec filename =
  let* script = Parse.Script.from_file filename in
  exec script

let cmd profiling debug optimize files no_exhaustion =
  let exec = Script.exec ~no_exhaustion ~optimize in
  if profiling then Log.profiling_on := true;
  if debug then Log.debug_on := true;
  list_iter (run_file exec) files
