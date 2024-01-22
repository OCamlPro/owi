(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021 Léo Andrès *)
(* Copyright © 2021 Pierre Chambart *)

open Syntax

let optimize_file ~unsafe filename =
  if not @@ Sys.file_exists filename then
    error_s "file `%s` doesn't exist" filename
  else
    let* modul = Parse.Module.from_file ~filename in
    Compile.until_optimize ~unsafe ~optimize:true modul

let cmd_one debug unsafe file =
  if debug then Log.debug_on := true;
  match optimize_file ~unsafe file with
  | Ok modul -> Format.pp_std "%a@\n" Simplified.Pp.modul modul
  | Error e ->
    Format.pp_err "%s@." e;
    exit 1

let cmd debug unsafe files = List.iter (cmd_one debug unsafe) files
