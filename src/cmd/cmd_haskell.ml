(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax

let compile ~workspace ~out_file (files : Fpath.t list) : Fpath.t Result.t =
  let* haskell_bin =
    let name = "wasm32-wasi-ghc" in
    match Bos.OS.Cmd.resolve @@ Bos.Cmd.v name with
    | Error _ ->
      Fmt.error_msg
        "The `%s` binary was not found, please make sure it is in your path."
        name
    | Ok _ as ok -> ok
  in

  let out = Option.value ~default:Fpath.(workspace / "out.wasm") out_file in
  let haskell : Bos.Cmd.t =
    Bos.Cmd.(
      haskell_bin
      (* output and input *)
      % "-o"
      % p out
      %% Bos.Cmd.of_list (List.map p files)
      (* % p libhaskell *) )
  in

  let err =
    match Logs.Src.level Log.main_src with
    | Some (Logs.Debug | Logs.Info) -> Bos.OS.Cmd.err_run_out
    | None | Some _ -> Bos.OS.Cmd.err_null
  in

  let+ () =
    Log.bench_fn "compiling time" @@ fun () ->
    match Bos.OS.Cmd.run ~err haskell with
    | Ok _ as v -> v
    | Error (`Msg e) ->
      Log.debug (fun m -> m "haskell failed: %s" e);
      Fmt.error_msg
        "haskell failed: run with -vv to get the full error message if it was \
         not displayed above"
  in

  out

let sym ~entry_point ~files ~out_file ~symbolic_parameters ~workspace :
  unit Result.t =
  let* source_file = compile ~workspace ~out_file files in

  Cmd_wasm_sym.cmd ~entry_point ~symbolic_parameters ~source_file ~workspace
