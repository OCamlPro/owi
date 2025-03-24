(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Bos
open Syntax

let compile ~entry_point ~includes debug (files : Fpath.t list) :
  Fpath.t Result.t =
  let entry_point = Option.value entry_point ~default:"_start" in
  let includes =
    Cmd.of_list (List.map (fun p -> Fmt.str "-I%a" Fpath.pp p) includes)
  in

  let* zig_bin = OS.Cmd.resolve @@ Cmd.v "zig" in

  let* libzig = Cmd_utils.find_installed_zig_file (Fpath.v "libzig.o") in

  match files with
  | [] -> assert false
  | [ file ] -> begin
    let out = Fpath.set_ext ".wasm" file |> Fpath.filename |> Fpath.v in
    let entry = Fmt.str "-fentry=%s" entry_point in
    let zig : Cmd.t =
      Cmd.(
        zig_bin % "build-exe" % "-target" % "wasm32-freestanding" % entry
        %% includes % p file % p libzig )
    in

    let+ () =
      match
        OS.Cmd.run
          ~err:(if debug then OS.Cmd.err_run_out else OS.Cmd.err_null)
          zig
      with
      | Ok _ as v -> v
      | Error (`Msg e) ->
        Fmt.error_msg "zig failed: %s"
          (if debug then e else "run with --debug to get the full error message")
    in

    out
  end
  | _ -> Fmt.failwith "TODO"

let cmd ~debug ~workers ~includes ~files ~profiling ~unsafe ~optimize
  ~no_stop_at_failure ~no_value ~no_assert_failure_expression_printing
  ~deterministic_result_order ~fail_mode ~concolic ~solver ~profile
  ~model_output_format ~entry_point : unit Result.t =
  let includes = Cmd_utils.zig_files_location @ includes in
  let* modul = compile ~entry_point ~includes debug files in
  let workspace = Fpath.v "owi-out" in
  let files = [ modul ] in
  (if concolic then Cmd_conc.cmd else Cmd_sym.cmd)
    ~profiling ~debug ~unsafe ~rac:false ~srac:false ~optimize ~workers
    ~no_stop_at_failure ~no_value ~no_assert_failure_expression_printing
    ~deterministic_result_order ~fail_mode ~workspace ~solver ~files ~profile
    ~model_output_format ~entry_point
