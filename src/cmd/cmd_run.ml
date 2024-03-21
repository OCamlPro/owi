(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021 Léo Andrès *)
(* Copyright © 2021 Pierre Chambart *)

open Syntax

let run_file ~unsafe ~optimize filename =
  let* modul = Parse.Module.from_file filename in
  let name = None in
  let+ (_state : Concrete_value.Func.extern_func Link.state) =
    Compile.until_interpret Link.empty_state ~unsafe ~optimize ~name modul
  in
  ()

let cmd profiling debug unsafe optimize files =
  if profiling then Log.profiling_on := true;
  if debug then Log.debug_on := true;
  list_iter (run_file ~unsafe ~optimize) files
