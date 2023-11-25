(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021 Léo Andrès *)
(* Copyright © 2021 Pierre Chambart *)

open Syntax

let format_file ~unsafe filename =
  if not @@ Sys.file_exists filename then
    error_s "file `%s` doesn't exist" filename
  else
    let* modul = Parse.Module.from_file ~filename in
    Compile.until_check ~unsafe modul

let cmd debug unsafe inplace (file : string) =
  if debug then Log.debug_on := true;
  match format_file ~unsafe file with
  | Ok modul ->
    if inplace then
      let chan = open_out file in
      Fun.protect
        ~finally:(fun () -> close_out chan)
        (fun () ->
          let fmt = Stdlib.Format.formatter_of_out_channel chan in
          Format.pp fmt "%a@\n" Text.pp_modul modul )
    else Format.pp_std "%a@\n" Text.pp_modul modul
  | Error e ->
    Format.pp_err "%s@." e;
    exit 1
