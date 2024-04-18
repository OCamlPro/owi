(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax

let optimize_file ~unsafe filename =
  let* modul = Parse.Module.from_file filename in
  Compile.until_optimize ~unsafe ~optimize:true modul

let cmd debug unsafe files =
  if debug then Log.debug_on := true;
  list_iter
    (fun file ->
      match optimize_file ~unsafe file with
      | Ok modul ->
        Format.pp_std "%a@\n" Simplified.Pp.modul modul;
        Ok ()
      | Error _ as e -> e )
    files
