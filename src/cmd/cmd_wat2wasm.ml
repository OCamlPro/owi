(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax

let cmd_one ~unsafe ~optimize outfile file =
  let ext = Fpath.get_ext file in
  match ext with
  | ".wat" ->
    let* modul = Parse.Text.Module.from_file file in
    Binary_encoder.convert outfile file ~unsafe ~optimize modul
  | ext -> Error (`Unsupported_file_extension ext)

let cmd profiling debug unsafe optimize outfile file =
  if profiling then Log.profiling_on := true;
  if debug then Log.debug_on := true;
  cmd_one ~unsafe ~optimize outfile file
