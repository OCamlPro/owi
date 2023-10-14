(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021 Léo Andrès *)
(* Copyright © 2021 Pierre Chambart *)

open Syntax

let simplify_then_link_then_run ~unsafe ~optimize file =
  let* to_run, link_state =
    list_fold_left
      (fun ((to_run, state) as acc) instruction ->
        match instruction with
        | Text.Module m ->
          let* m, state =
            Compile.until_link state ~unsafe ~optimize ~name:None m
          in
          Ok (m :: to_run, state)
        | Text.Register (name, id) ->
          let* state = Link.register_module state ~name ~id in
          Ok (to_run, state)
        | _ -> Ok acc )
      ([], Link.empty_state) file
  in
  list_iter (Interpret.Concrete.modul link_state.envs) (List.rev to_run)

let run_file exec filename =
  if not @@ Sys.file_exists filename then
    error_s "file `%s` doesn't exist" filename
  else
    let* script = Parse.Script.from_file ~filename in
    exec script

let cmd profiling debug unsafe optimize files =
  let exec = simplify_then_link_then_run ~unsafe ~optimize in
  if profiling then Log.profiling_on := true;
  if debug then Log.debug_on := true;
  let result = list_iter (run_file exec) files in
  match result with
  | Ok () -> ()
  | Error e ->
    Format.eprintf "%s@." e;
    exit 1
