(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax

(** Utility function to handle writing to a file or printing to stdout *)
let cmd_one emitfile outfile file =
  let ext = Fpath.get_ext file in
  let _dir, wat_file = Fpath.split_base file in
  let wat_file = Fpath.set_ext "wat" wat_file in

  match ext with
  | ".wasm" ->
    let* m = Parse.Binary.Module.from_file file in
    let m = Binary_to_text.modul m in
    let outname, output =
      begin
        match outfile with
        | Some name -> (name, true)
        | None -> (wat_file, false)
      end
    in
    if emitfile || output then
      Bos.OS.File.writef outname "%a@\n" Text.pp_modul m
    else Ok (Fmt.pr "%a@\n" Text.pp_modul m)
  | ext -> Error (`Unsupported_file_extension ext)

let cmd file emitfile outfile = cmd_one emitfile outfile file
