(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

open Result

exception Interrupt

let () =
  Sys.(set_signal sigint (Signal_handle (fun _ -> raise Interrupt)))

let read_line_stdin () =
  let rec read_chars acc =
    let buf = Bytes.create 1 in
    match Unix.read Unix.stdin buf 0 1 with
    | 0 -> raise End_of_file
    | _ ->
        let byte = Bytes.get_uint8 buf 0 in
        if byte = 10 then
          String.concat "" (List.rev acc)
        else
          read_chars (String.make 1 (Char.chr byte) :: acc)
  in
  read_chars []

let cmd () =
  let env = Concrete_env.empty () in
  let module I = Interpret.Concrete (Interpret.Default_parameters) in

  let rec loop env =
    Fmt.pr "> %!";
    match
      try Some (read_line_stdin ())
      with End_of_file -> None | Interrupt -> Fmt.pr "\n"; None
    with
    | None ->
        Log.app (fun m -> m "Bye!");
        Ok ()
    | Some raw_input ->
        let input = String.trim raw_input in
        if String.equal input "quit" || String.equal input "exit" then (
          Log.app (fun m -> m "Bye!");
          Ok ()
        ) else if String.equal input "" then
          loop env
        else
          let temp_file = Fpath.v "repl_input.wat" in
          let content = String.concat "" [input; "\n"] in
          match Bos.OS.File.write temp_file content with
          | Error e ->
              Log.err (fun m -> m "Failed to write temp file: %s" (err_to_string e));
              loop env
          | Ok () ->
              match Compile.File.until_validate ~unsafe:false temp_file with
              | Error e ->
                  Log.err (fun m -> m "Parse error: %s" (err_to_string e));
                  loop env
              | Ok module_ ->
                  match Compile.Binary.until_concrete_link ~unsafe:false env ~name:None module_ with
                  | Ok (modul, env') ->
                      (match I.modul env' ~modul with
                       | Ok () -> Log.app (fun m -> m "OK")
                       | Error e -> Log.err (fun m -> m "Execution error: %s" (err_to_string e)));
                      loop env'
                  | Error e ->
                      Log.err (fun m -> m "Link/execution error: %s" (err_to_string e));
                      loop env
  in
  loop env