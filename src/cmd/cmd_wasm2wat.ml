(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax

(* Utility function to handle writing to a file or printing to stdout *)

let cmd_one emitfile file =
  let ext = Fpath.get_ext file in
  let wat_file = Fpath.set_ext "wat" file in

  match ext with
  | ".wasm" ->
    let* m = Parse.Binary.Module.from_file file in
    let m = Binary_to_text.modul m in
    let wat_module = Fmt.str "%a@\n" Text.pp_modul m in
    if emitfile then Bos.OS.File.write wat_file wat_module
    else Ok (Fmt.pr "%s" wat_module)
  | ext -> Error (`Unsupported_file_extension ext)

let cmd files emitfile = list_iter (cmd_one emitfile) files
