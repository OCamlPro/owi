(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021 Léo Andrès *)
(* Copyright © 2021 Pierre Chambart *)

open Syntax

let validate filename =
  let* modul = Parse.Module.from_file filename in
  let* _modul = Compile.until_typecheck ~unsafe:false modul in
  Ok ()

let cmd debug files =
  if debug then Log.debug_on := true;
  let result = list_iter validate files in
  match result with
  | Ok () -> ()
  | Error e ->
    Format.pp_err "%s@\n" e;
    exit 1
