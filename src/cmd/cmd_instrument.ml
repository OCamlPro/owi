(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax

let cmd_one unsafe symbolic file =
  let _dir, filename = Fpath.split_base file in
  let filename, ext = Fpath.split_ext filename in
  match ext with
  | ".wat" ->
    let* text_modul = Parse.Text.Module.from_file file in
    let* instrumented_binary_modul =
      Compile.Text.until_binary ~unsafe ~rac:true ~srac:symbolic text_modul
    in
    let instrumented_text_modul =
      Binary_to_text.modul instrumented_binary_modul
    in
    let filename = Fpath.add_ext ".instrumented" filename in
    let filename = Fpath.add_ext ".wat" filename in
    let* () =
      Binary_encoder.convert None filename ~unsafe ~optimize:false
        instrumented_text_modul
    in
    Bos.OS.File.writef filename "%a" Text.pp_modul instrumented_text_modul
  | ext -> Error (`Unsupported_file_extension ext)

let cmd debug unsafe symbolic files =
  if debug then Log.debug_on := true;
  list_iter (cmd_one unsafe symbolic) files
