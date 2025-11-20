(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax

let get_printer filename =
  let ext = Fpath.get_ext filename in
  match ext with
  | ".wat" ->
    let+ v = Parse.Text.Module.from_file filename in
    fun fmt () -> Text.Module.pp fmt v
  | ".wast" ->
    let+ v = Parse.Text.Script.from_file filename in
    fun fmt () -> Wast.pp_script fmt v
  | ext -> Error (`Unsupported_file_extension ext)

let cmd_one inplace file =
  let* pp = get_printer file in
  if inplace then Bos.OS.File.writef file "%a@\n" pp ()
  else begin
    Log.app (fun m -> m "%a" pp ());
    Ok ()
  end

let cmd ~inplace ~files = list_iter (cmd_one inplace) files
