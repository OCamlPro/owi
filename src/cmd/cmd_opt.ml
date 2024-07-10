(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax

let optimize_file ~unsafe filename =
  Compile.File.until_optimize ~unsafe ~optimize:true filename

let cmd debug unsafe files =
  if debug then Log.debug_on := true;
  list_iter
    (fun file ->
      let+ m = optimize_file ~unsafe file in
      let m = Binary_to_text.modul m in
      Format.pp_std "%a@\n" Text.pp_modul m )
    files
