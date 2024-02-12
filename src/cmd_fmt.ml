(* SPDX-License-Identifier: AGPL-3.0-or-later *)

open Syntax

let get_printer filename =
  let ext = Fpath.get_ext filename in
  match ext with
  | ".wat" ->
    let+ v = Parse.Module.from_file filename in
    fun fmt () -> Text.pp_modul fmt v
  | ".wast" ->
    let+ v = Parse.Script.from_file filename in
    fun fmt () -> Text.pp_script fmt v
  | _ -> error_s "unsupported file extension"

let cmd_one inplace file =
  match get_printer file with
  | Error e ->
    Format.pp_err "%s@." e;
    exit 1
  | Ok pp ->
    if inplace then
      let* res =
        Bos.OS.File.with_oc file
          (fun chan () ->
            let fmt = Stdlib.Format.formatter_of_out_channel chan in
            Ok (Format.pp fmt "%a@\n" pp ()) )
          ()
      in
      res
    else Ok (Format.pp_std "%a@\n" pp ())

let cmd inplace files =
  match list_iter (cmd_one inplace) files with
  | Ok () -> ()
  | Error (`Msg e) -> failwith e

let format_file_to_string file =
  let+ pp = get_printer file in
  Format.asprintf "%a@\n" pp ()
