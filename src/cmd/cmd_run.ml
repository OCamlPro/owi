(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax

let run_file ~unsafe ~optimize filename =
  let* m = Parse.guess_from_file filename in
  let name = None in
  match m with
  | Either.Left (Either.Left text_module) ->
    let+ (_state : Concrete_value.Func.extern_func Link.state) =
      Compile.Text.until_interpret Link.empty_state ~unsafe ~optimize ~name
        text_module
    in
    ()
  | Either.Left (Either.Right _text_script) ->
    (* TODO: merge script and run cmd together and call script here *)
    assert false
  | Either.Right binary_module ->
    let+ (_state : Concrete_value.Func.extern_func Link.state) =
      Compile.Binary.until_interpret Link.empty_state ~unsafe ~optimize ~name
        binary_module
    in
    ()

let cmd profiling debug unsafe optimize files =
  if profiling then Log.profiling_on := true;
  if debug then Log.debug_on := true;
  list_iter (run_file ~unsafe ~optimize) files
