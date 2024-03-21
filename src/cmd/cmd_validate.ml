(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021 Léo Andrès *)
(* Copyright © 2021 Pierre Chambart *)

open Syntax

let validate filename =
  let* modul = Parse.Module.from_file filename in
  let+ _modul = Compile.until_typecheck ~unsafe:false modul in
  ()

let cmd debug files =
  if debug then Log.debug_on := true;
  list_iter validate files
