(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax

let resolve_binary name =
  match Bos.OS.Cmd.resolve @@ Bos.Cmd.v name with
  | Error _ ->
    Fmt.error_msg
      "The `%s` binary was not found, please make sure it is in your path." name
  | Ok _ as ok -> ok

let err_output =
  match Logs.Src.level Log.main_src with
  | Some (Logs.Debug | Logs.Info) -> Bos.OS.Cmd.err_run_out
  | None | Some _ -> Bos.OS.Cmd.err_null

let bitcode_of_input ~workspace ~llvm_as_bin file : Fpath.t Result.t =
  match Fpath.get_ext ~multi:false file with
  | ".bc" -> Ok file
  | ".ll" ->
    let out_bc = Fpath.(workspace // Fpath.base (file -+ ".bc")) in
    let llvm_as_cmd : Bos.Cmd.t =
      Bos.Cmd.(llvm_as_bin % p file % "-o" % p out_bc)
    in
    let+ () =
      match Bos.OS.Cmd.run ~err:err_output llvm_as_cmd with
      | Ok _ as v -> v
      | Error (`Msg e) ->
        Log.debug (fun m -> m "llvm-as failed: %s" e);
        Fmt.error_msg
          "llvm-as failed: run with -vv to get the full error message if it \
           was not displayed above"
    in
    out_bc
  | ext ->
    Fmt.error_msg
      "Unsupported file extension `%s` for LLVM command, expected .ll or .bc"
      ext

let compile ~workspace ~entry_point ~out_file (files : Fpath.t list) :
  Fpath.t Result.t =
  let* llvm_as_bin = resolve_binary "llvm-as" in
  let* llc_bin = resolve_binary "llc" in
  let* wasmld_bin = resolve_binary "wasm-ld" in

  let* bc_files = list_map (bitcode_of_input ~workspace ~llvm_as_bin) files in

  let files_bc = Bos.Cmd.of_list (List.map Bos.Cmd.p bc_files) in
  let llc_cmd : Bos.Cmd.t =
    Bos.Cmd.(
      llc_bin % "-O0" % "-march=wasm32" % "-mtriple=wasm32-unknown-unknown"
      % "-filetype=obj" %% files_bc )
  in

  let* () =
    Log.bench_fn "llc time" @@ fun () ->
    match Bos.OS.Cmd.run ~err:err_output llc_cmd with
    | Ok _ as v -> v
    | Error (`Msg e) ->
      Log.debug (fun m -> m "llc failed: %s" e);
      Fmt.error_msg "llc failed: run with -vv to get the full error message"
  in

  let files_o =
    Bos.Cmd.of_list
      (List.map (fun file -> Bos.Cmd.p Fpath.(file -+ ".o")) bc_files)
  in

  let out = Option.value ~default:Fpath.(workspace / "a.out.wasm") out_file in

  let* libc = Cmd_utils.find_installed_c_file (Fpath.v "libc.wasm") in
  let* libowi = Cmd_utils.find_installed_c_file (Fpath.v "libowi.wasm") in

  let wasmld_cmd : Bos.Cmd.t =
    Bos.Cmd.(
      wasmld_bin
      %% of_list
           ( [ "-z"; "stack-size=8388608" ]
           @ ( match entry_point with
             | None -> []
             | Some entry_point ->
               [ Fmt.str "--export=%s" entry_point
               ; Fmt.str "--entry=%s" entry_point
               ] )
           @ [ "--allow-undefined" ]
           @ [ p libc; p libowi ]
           @ [ "-o"; p out ] )
      %% files_o )
  in

  let+ () =
    Log.bench_fn "wasm-ld time" @@ fun () ->
    match Bos.OS.Cmd.run ~err:err_output wasmld_cmd with
    | Ok _ as v -> v
    | Error (`Msg e) ->
      Log.debug (fun m -> m "wasm-ld failed: %s" e);
      Fmt.error_msg
        "wasm-ld failed: run with -vv to get the full error message if it was \
         not displayed above"
  in

  out

let sym ~entry_point ~files ~out_file ~symbolic_parameters ~workspace :
  unit Result.t =
  let* source_file = compile ~workspace ~entry_point ~out_file files in

  Cmd_wasm_sym.cmd ~entry_point ~source_file ~symbolic_parameters ~workspace
