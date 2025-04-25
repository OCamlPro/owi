(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax

let cmd_one ~unsafe ~optimize ~out_file ~source_file =
  let ext = Fpath.get_ext source_file in
  match ext with
  | ".wat" ->
    let* modul = Parse.Text.Module.from_file source_file in
    Binary_encoder.convert out_file source_file ~unsafe ~optimize modul
  | ext -> Error (`Unsupported_file_extension ext)

let cmd ~unsafe ~optimize ~out_file ~source_file =
  cmd_one ~unsafe ~optimize ~out_file ~source_file
