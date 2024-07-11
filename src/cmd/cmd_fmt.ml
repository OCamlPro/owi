(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax

let get_printer filename =
  let ext = Fpath.get_ext filename in
  match ext with
  | ".wat" ->
    let+ v = Parse.Text.Module.from_file filename in
    fun fmt () -> Text.pp_modul fmt v
  | ".wast" ->
    let+ v = Parse.Text.Script.from_file filename in
    fun fmt () -> Text.pp_script fmt v
  | ext -> Error (`Unsupported_file_extension ext)

let cmd_one inplace file =
  match get_printer file with
  | Error _e as e -> e
  | Ok pp ->
    if inplace then Bos.OS.File.writef file "%a@\n" pp ()
    else Ok (Fmt.pr "%a@\n" pp ())

let cmd inplace files = list_iter (cmd_one inplace) files

let format_file_to_string file =
  let+ pp = get_printer file in
  Fmt.str "%a@\n" pp ()
