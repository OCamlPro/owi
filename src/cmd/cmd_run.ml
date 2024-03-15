(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax

let run_wat_file ~unsafe ~optimize filename =
  let* modul = Parse.Module.from_file filename in
  let name = None in
  let+ (_state : Concrete_value.Func.extern_func Link.state) =
    Compile.until_interpret Link.empty_state ~unsafe ~optimize ~name modul
  in
  ()

let run_wasm_file ~unsafe ~optimize filename =
  let* modul = Binary_deserializer.from_file filename in
  let name = None in
  let+ (_state : Concrete_value.Func.extern_func Link.state) =
    Compile.simplified_interpret Link.empty_state ~unsafe ~optimize ~name modul
  in
  ()

let run_file ~unsafe ~optimize filename =
  if Fpath.has_ext "wasm" filename then run_wasm_file ~unsafe ~optimize filename
  else if Fpath.has_ext "wat" filename then
    run_wat_file ~unsafe ~optimize filename
  else Error (`Unsupported_file_extension (Fpath.filename filename))

let cmd profiling debug unsafe optimize files =
  if profiling then Log.profiling_on := true;
  if debug then Log.debug_on := true;
  list_iter (run_file ~unsafe ~optimize) files
