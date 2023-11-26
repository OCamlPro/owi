(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021 Léo Andrès *)
(* Copyright © 2021 Pierre Chambart *)

open Syntax

let read_script filename =
  if not @@ Sys.file_exists filename then
    error_s "file `%s` doesn't exist" filename
  else Parse.Script.from_file ~filename

let cmd debug _unsafe inplace (file : string) =
  if debug then Log.debug_on := true;
  match read_script file with
  | Ok script ->
    if inplace then
      let chan = open_out file in
      Fun.protect
        ~finally:(fun () -> close_out chan)
        (fun () ->
          let fmt = Stdlib.Format.formatter_of_out_channel chan in
          Format.pp fmt "%a@\n" Text.pp_script script )
    else Format.pp_std "%a@\n" Text.pp_script script
  | Error e ->
    Format.pp_err "%s@." e;
    exit 1
