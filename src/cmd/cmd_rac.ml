(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax

let cmd_one unsafe file =
  let _dir, filename = Fpath.split_base file in
  let filename, ext = Fpath.split_ext filename in
  match ext with
  | ".wat" ->
    let* text_modul = Parse.Text.Module.from_file file in
    let* binary_modul = Compile.Text.until_binary ~unsafe text_modul in
    let+ instrumented_binary_modul = Code_generator.generate binary_modul in
    let instrumented_text_modul =
      Binary_to_text.modul instrumented_binary_modul
    in

    let content = Fmt.str "%a" Text.pp_modul instrumented_text_modul in
    let filename = Fpath.add_ext ".instrumented" filename in
    let filename = Fpath.add_ext ".wat" filename in
    let filename = Fpath.to_string filename in
    let oc = Out_channel.open_bin filename in
    Out_channel.output_string oc content;
    Out_channel.close oc
  | ext -> Error (`Unsupported_file_extension ext)

let cmd unsafe files = list_iter (cmd_one unsafe) files
