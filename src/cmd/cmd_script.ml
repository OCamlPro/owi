(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax

let run_file exec filename =
  let* script = Parse.Text.Script.from_file filename in
  exec script

let cmd ~files ~no_exhaustion =
  let exec = Script.exec ~no_exhaustion in
  list_iter (run_file exec) files
