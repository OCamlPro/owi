(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax

let optimize_file ~unsafe filename =
  let* m = Parse.guess_from_file filename in
  match m with
  | Either.Left (Either.Left modul) ->
    Compile.Text.until_optimize ~unsafe ~optimize:true modul
  | Either.Left (Either.Right _script) ->
    Error (`Msg "script can't be optimised")
  | Either.Right modul ->
    Compile.Binary.until_optimize ~unsafe ~optimize:true modul

let cmd debug unsafe files =
  if debug then Log.debug_on := true;
  list_iter
    (fun file ->
      match optimize_file ~unsafe file with
      | Ok m ->
        let m = Binary_to_text.modul m in
        Format.pp_std "%a@\n" Text.pp_modul m;
        Ok ()
      | Error _ as e -> e )
    files
