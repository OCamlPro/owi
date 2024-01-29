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

let run_file ~unsafe ~optimize filename =
  let* script = Parse.Script.from_file filename in
  simplify_then_link_then_run ~unsafe ~optimize script

let cmd profiling debug unsafe optimize files =
  if profiling then Log.profiling_on := true;
  if debug then Log.debug_on := true;
  let result = list_iter (run_file ~unsafe ~optimize) files in
  match result with
  | Ok () -> ()
  | Error e ->
    Format.pp_err "%s@\n" e;
    exit 1
