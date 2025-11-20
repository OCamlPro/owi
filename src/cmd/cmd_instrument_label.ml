(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax

let cmd ~unsafe ~coverage_criteria ~source_file =
  let _dir, filename = Fpath.split_base source_file in
  let filename, ext = Fpath.split_ext filename in

  let* m = Compile.File.until_binary ~unsafe source_file in

  let m = Label.annotate coverage_criteria m in

  (* TODO: this should not be required in the binary case but for some reason Binary_encoder.convert expect a text module... *)
  let m = Binary_to_text.modul m in

  let filename = Fpath.add_ext ".instrumented" filename in

  match ext with
  | ".wat" ->
    let filename = Fpath.add_ext ".wat" filename in
    Bos.OS.File.writef filename "%a" Text.Module.pp m
  | ".wasm" ->
    let filename = Fpath.add_ext ".wasm" filename in
    Binary_encoder.convert None filename ~unsafe m
  | ext -> Error (`Unsupported_file_extension ext)
