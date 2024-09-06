(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax

let run_file ~unsafe ~rac ~optimize filename =
  let name = None in
  let+ (_ : _ Link.state) =
    Compile.File.until_interpret ~unsafe ~rac ~srac:false ~optimize ~name
      Link.empty_state filename
  in
  ()

let cmd profiling debug unsafe rac optimize files =
  if profiling then Log.profiling_on := true;
  if debug then Log.debug_on := true;
  list_iter (run_file ~unsafe ~rac ~optimize) files
