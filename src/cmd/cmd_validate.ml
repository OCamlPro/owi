(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax

let validate filename =
  let* modul = Parse.guess_from_file filename in
  match modul with
  | Either.Left (Either.Left text_module) ->
    let+ _modul = Compile.Text.until_typecheck ~unsafe:false text_module in
    ()
  | Either.Left (Either.Right _text_script) ->
    Error (`Msg "can not run validation on a script (.wast) file")
  | Either.Right binary_module ->
    let+ _module = Compile.Binary.until_typecheck ~unsafe:false binary_module in
    ()

let cmd debug files =
  if debug then Log.debug_on := true;
  list_iter validate files
