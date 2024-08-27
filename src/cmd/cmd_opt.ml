(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax

let optimize_file ~unsafe filename =
  Compile.File.until_optimize ~unsafe ~optimize:true filename

let print_or_emit ~unsafe file outfile =
  let+ m = optimize_file ~unsafe file in
  let m = Binary_to_text.modul m in
  match outfile with
  | Some name -> Bos.OS.File.writef name "%a@\n" Text.pp_modul m
  | None -> Ok (Fmt.pr "%a@\n" Text.pp_modul m)

let cmd debug unsafe file outfile =
  if debug then Log.debug_on := true;
  Result.bind (print_or_emit ~unsafe file outfile) (fun _ -> Ok ())
