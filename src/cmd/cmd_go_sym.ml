(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax

let compile ~workspace ~out_file (files : Fpath.t list) : Fpath.t Result.t =
  let* tinygo_bin =
    let name = "tinygo" in
    match Bos.OS.Cmd.resolve @@ Bos.Cmd.v name with
    | Error _ ->
      Fmt.error_msg
        "The `%s` binary was not found, please make sure it is in your path."
        name
    | Ok _ as ok -> ok
  in

  let out = Option.value ~default:Fpath.(workspace / "out.wasm") out_file in
  let tinygo : Bos.Cmd.t =
    Bos.Cmd.(
      tinygo_bin % "build" % "-target" % "wasm" % "-no-debug" % "-opt" % "2"
      % "-panic" % "trap"
      (* initialization time is way too slow otherwise *)
      % "-gc"
      % "leaking"
      (* output and input *)
      % "-o"
      % p out
      %% Bos.Cmd.of_list (List.map p files)
      (* % p libtinygo *) )
  in

  let err =
    match Logs.Src.level Log.main_src with
    | Some (Logs.Debug | Logs.Info) -> Bos.OS.Cmd.err_run_out
    | None | Some _ -> Bos.OS.Cmd.err_null
  in

  let+ () =
    Log.bench_fn "compiling time" @@ fun () ->
    match Bos.OS.Cmd.run ~err tinygo with
    | Ok _ as v -> v
    | Error (`Msg e) ->
      Log.debug (fun m -> m "tinygo failed: %s" e);
      Fmt.error_msg
        "tinygo failed: run with -vv to get the full error message if it was \
         not displayed above"
  in

  out

let cmd ~entry_point ~files ~out_file
  ~(symbolic_parameters : Symbolic_parameters.t) : unit Result.t =
  let* workspace =
    Cmd_utils.make_workspace ~workspace:symbolic_parameters.workspace
  in

  let* source_file = compile ~workspace ~out_file files in
  let workspace = Some workspace in

  let symbolic_parameters = { symbolic_parameters with workspace } in

  Cmd_wasm_sym.cmd ~entry_point ~source_file ~symbolic_parameters
