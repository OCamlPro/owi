(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Bos
open Syntax

let compile ~workspace ~entry_point ~includes ~opt_lvl ~out_file debug
  (files : Fpath.t list) : Fpath.t Result.t =
  let* clangpp_bin = OS.Cmd.resolve @@ Cmd.v "clang++" in
  let opt_lvl = Fmt.str "-O%s" opt_lvl in

  let includes = Cmd.of_list ~slip:"-I" (List.map Cmd.p includes) in

  let err = if debug then OS.Cmd.err_run_out else OS.Cmd.err_null in
  let* () =
    (* TODO: we use this recursive function in order to be able to use `-o` on
       each file. We could get rid of this if we managed to call the C++
       compiler and the linker in the same step as it is done for C - then
       there would be a single output file and we could use `-o` more easily. *)
    let rec compile_files = function
      | [] -> Ok ()
      | file :: rest -> (
        let out_bc = Fpath.(workspace // Fpath.base (file -+ ".bc")) in
        let clang_cmd =
          Cmd.(
            clangpp_bin % "-Wno-everything" % opt_lvl % "-emit-llvm"
            % "--target=wasm32" % "-m32" % "-c" %% includes % "-o" % p out_bc
            % p file )
        in
        match OS.Cmd.run ~err clang_cmd with
        | Ok _ -> compile_files rest
        | Error (`Msg e) ->
          Fmt.error_msg "clang++ failed: %s"
            ( if debug then e
              else "run with --debug to get the full error message" ) )
    in
    compile_files files
  in

  let* llc_bin = OS.Cmd.resolve @@ Cmd.v "llc" in

  let files_bc =
    Cmd.of_list
    @@ List.map
         (fun file -> Fpath.(workspace // Fpath.base (file -+ ".bc")) |> Cmd.p)
         files
  in

  let llc_cmd : Cmd.t =
    Cmd.(
      llc_bin
      %
      (* TODO: configure this ? *)
      "-O0" % "-march=wasm32" % "-filetype=obj" %% files_bc )
  in

  let* () =
    match OS.Cmd.run ~err llc_cmd with
    | Ok _ as v -> v
    | Error (`Msg e) ->
      Fmt.error_msg "llc failed: %s"
        (if debug then e else "run with --debug to get the full error message")
  in
  let* wasmld_bin = OS.Cmd.resolve @@ Cmd.v "wasm-ld" in

  let files_o =
    Cmd.of_list
    @@ List.map
         (fun file -> Fpath.(workspace // Fpath.base (file -+ ".o")) |> Cmd.p)
         files
  in

  let out =
    Option.value ~default:Fpath.(workspace // v "a.out.wasm") out_file
  in

  let* binc = Cmd_utils.find_installed_c_file (Fpath.v "libc.wasm") in
  let wasmld_cmd : Cmd.t =
    Cmd.(
      wasmld_bin % "-z" % "stack-size=8388608"
      % Fmt.str "--export=%s" entry_point
      % Fmt.str "--entry=%s" entry_point
      %% files_o % p binc % "-o" % p out )
  in

  let+ () =
    match OS.Cmd.run ~err wasmld_cmd with
    | Ok _ as v -> v
    | Error (`Msg e) ->
      Fmt.error_msg "wasm-ld failed: %s"
        (if debug then e else "run with --debug to get the full error message")
  in

  out

let cmd ~debug ~arch:_ ~workers ~opt_lvl ~includes ~files ~profiling ~unsafe
  ~optimize ~no_stop_at_failure ~no_value ~no_assert_failure_expression_printing
  ~deterministic_result_order ~fail_mode ~concolic ~solver ~profile
  ~model_format ~entry_point ~invoke_with_symbols ~out_file ~workspace :
  unit Result.t =
  let* workspace =
    match workspace with
    | Some path -> Ok path
    | None -> OS.Dir.tmp "owi_cpp_%s"
  in
  let* _did_create : bool = OS.Dir.create ~path:true workspace in

  let entry_point = Option.value entry_point ~default:"main" in
  let includes = Cmd_utils.c_files_location @ includes in
  let* modul =
    compile ~workspace ~entry_point ~includes ~opt_lvl ~out_file debug files
  in
  let files = [ modul ] in
  let entry_point = Some entry_point in
  let workspace = Some workspace in
  (if concolic then Cmd_conc.cmd else Cmd_sym.cmd)
    ~profiling ~debug ~unsafe ~rac:false ~srac:false ~optimize ~workers
    ~no_stop_at_failure ~no_value ~no_assert_failure_expression_printing
    ~deterministic_result_order ~fail_mode ~workspace ~solver ~files ~profile
    ~model_format ~entry_point ~invoke_with_symbols
