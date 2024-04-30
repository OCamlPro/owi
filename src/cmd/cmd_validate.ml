(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax

let validate filename =
  let* modul = Parse.Text.Module.from_file filename in
  let+ _modul = Compile.Text.until_typecheck ~unsafe:false modul in
  ()

let cmd debug files =
  if debug then Log.debug_on := true;
  list_iter validate files
