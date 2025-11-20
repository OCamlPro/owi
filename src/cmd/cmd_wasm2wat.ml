(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax

(** Utility function to handle writing to a file or printing to stdout *)
let cmd ~source_file ~emit_file ~out_file =
  let ext = Fpath.get_ext source_file in
  match ext with
  | ".wasm" ->
    let _dir, wat_file = Fpath.split_base source_file in
    let wat_file = Fpath.set_ext "wat" wat_file in
    let* m = Parse.Binary.Module.from_file source_file in
    let m = Binary_to_text.modul m in
    let outname, output =
      begin match out_file with
      | Some name -> (name, true)
      | None -> (wat_file, false)
      end
    in
    if emit_file || output then
      Bos.OS.File.writef outname "%a@\n" Text.Module.pp m
    else begin
      Log.app (fun log -> log "%a" Text.Module.pp m);
      Ok ()
    end
  | ext -> Error (`Unsupported_file_extension ext)
