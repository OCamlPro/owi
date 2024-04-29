(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax

let cmd_one file =
  let ext = Fpath.get_ext file in
  match ext with
  | ".wasm" ->
    let* m = Parse.Binary.Module.from_file file in
    Ok (Format.pp_std "%a@\n" Simplified.Pp.modul m)
  | ext -> Error (`Msg (Format.sprintf "invalid extension: `%s`" ext))

let cmd files = list_iter cmd_one files
