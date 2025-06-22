(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Bos
open Syntax

let compile ~workspace ~out_file (files : Fpath.t list) : Fpath.t Result.t =
  let* tinygo_bin =
    let name = "tinygo" in
    match OS.Cmd.resolve @@ Cmd.v name with
    | Error _ ->
      Fmt.error_msg
        "The `%s` binary was not found, please make sure it is in your path."
        name
    | Ok _ as ok -> ok
  in

  let out = Option.value ~default:Fpath.(workspace / "out.wasm") out_file in
  let tinygo : Cmd.t =
    Cmd.(
      tinygo_bin % "build" % "-target" % "wasm" % "-no-debug" % "-opt" % "2"
      % "-panic" % "trap" % "-gc" % "leaking" % "-o" % p out
      %% Cmd.of_list (List.map p files)
      (* % p libtinygo *) )
  in

  let err =
    match Logs.level () with
    | Some (Logs.Debug | Logs.Info) -> OS.Cmd.err_run_out
    | None | Some _ -> OS.Cmd.err_null
  in

  let+ () =
    match OS.Cmd.run ~err tinygo with
    | Ok _ as v -> v
    | Error (`Msg e) ->
      Logs.debug (fun m -> m "tinygo failed: %s" e);
      Fmt.error_msg
        "tinygo failed: run with -vv to get the full error message if it was \
         not displayed above"
  in

  out

let cmd ~symbolic_parameters ~files ~concolic ~out_file : unit Result.t =
  let* workspace =
    match symbolic_parameters.Cmd_sym.workspace with
    | Some path -> Ok path
    | None -> OS.Dir.tmp "cmd_tinygo_%s"
  in
  let* _did_create : bool = OS.Dir.create workspace in

  let* source_file = compile ~workspace ~out_file files in
  let workspace = Some workspace in

  let parameters = { symbolic_parameters with workspace } in

  (if concolic then Cmd_conc.cmd else Cmd_sym.cmd) ~parameters ~source_file
