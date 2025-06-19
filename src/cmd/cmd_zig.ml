(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Bos
open Syntax

let compile ~workspace ~entry_point ~includes ~out_file (files : Fpath.t list) :
  Fpath.t Result.t =
  let includes =
    Cmd.of_list (List.map (fun p -> Fmt.str "-I%a" Fpath.pp p) includes)
  in

  let* zig_bin =
    match OS.Cmd.resolve @@ Cmd.v "zig" with
    | Error _ ->
      Error
        (`Msg
           "The `zig` binary was not found, please make sure it is in your \
            path." )
    | Ok _ as ok -> ok
  in

  (* TODO: disabled until zig is properly packaged everywhere
     let* libzig = Cmd_utils.find_installed_zig_file (Fpath.v "libzig.o") in
  *)
  let out = Option.value ~default:Fpath.(workspace / "out.wasm") out_file in
  let entry =
    match entry_point with
    | None -> ""
    | Some entry_point -> Fmt.str "-fentry=%s" entry_point
  in
  let zig : Cmd.t =
    Cmd.(
      zig_bin % "build-exe" % "-target" % "wasm32-freestanding"
      % Fmt.str "-femit-bin=%a" Fpath.pp out
      % entry %% includes
      %% Cmd.of_list (List.map p files)
      (* % p libzig *) )
  in

  let err =
    match Logs.level () with
    | Some (Logs.Debug | Logs.Info) -> OS.Cmd.err_run_out
    | None | Some _ -> OS.Cmd.err_null
  in

  let+ () =
    match OS.Cmd.run ~err zig with
    | Ok _ as v -> v
    | Error (`Msg e) ->
      Logs.debug (fun m -> m "zig failed: %s" e);
      Fmt.error_msg
        "zig failed: run with -vv to get the full error message if it was not \
         displayed above"
  in

  out

let cmd ~symbolic_parameters ~includes ~files ~concolic ~out_file :
  unit Result.t =
  let* workspace =
    match symbolic_parameters.Cmd_sym.workspace with
    | Some path -> Ok path
    | None -> OS.Dir.tmp "cmd_zig_%s"
  in
  let* _did_create : bool = OS.Dir.create workspace in

  let includes =
    (* TODO: disabled until zig is properly packaged
       Cmd_utils.zig_files_location @
    *)
    includes
  in
  let* source_file =
    compile ~workspace ~entry_point:symbolic_parameters.entry_point ~includes
      ~out_file files
  in
  let workspace = Some workspace in

  let parameters = { symbolic_parameters with workspace } in

  (if concolic then Cmd_conc.cmd else Cmd_sym.cmd) ~parameters ~source_file
