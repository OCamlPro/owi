(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax

let optimize_file ~unsafe ~source_file =
  Compile.File.until_optimize ~unsafe ~rac:false ~srac:false ~optimize:true
    source_file

let print_or_emit ~unsafe ~source_file ~out_file =
  let* m = optimize_file ~unsafe ~source_file in
  let m = Binary_to_text.modul m in
  match out_file with
  | Some name -> Bos.OS.File.writef name "%a@\n" Text.pp_modul m
  | None -> Ok (Fmt.pr "%a@\n" Text.pp_modul m)

let cmd ~debug ~unsafe ~source_file ~out_file =
  if debug then Log.debug_on := true;
  print_or_emit ~unsafe ~source_file ~out_file
