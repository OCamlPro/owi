(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Bos
open Syntax

(* TODO: refactor to re-use code in Cmd_c.ml *)
let binc_location = List.map Fpath.v Share.Sites.binc

let libc_location = List.map Fpath.v Share.Sites.libc

let find location file : Fpath.t Result.t =
  let* l =
    list_map
      (fun dir ->
        let filename = Fpath.append dir file in
        match OS.File.exists filename with
        | Ok true -> Ok (Some filename)
        | Ok false -> Ok None
        | Error (`Msg msg) -> Error (`Msg msg) )
      location
  in
  let rec loop = function
    | [] -> Error (`Msg (Fmt.str "can't find file %a" Fpath.pp file))
    | None :: tl -> loop tl
    | Some file :: _tl -> Ok file
  in
  loop l

let compile ~includes ~opt_lvl debug (files : Fpath.t list) : Fpath.t Result.t =
  let* clangpp_bin = OS.Cmd.resolve @@ Cmd.v "clang++" in
  let opt_lvl = Fmt.str "-O%s" opt_lvl in

  let includes = Cmd.of_list ~slip:"-I" (List.map Cmd.p includes) in

  let clang_cmd : Cmd.t =
    Cmd.(
      clangpp_bin % "-Wno-everything" % opt_lvl % "-emit-llvm"
      % "--target=wasm32" % "-m32" % "-c" %% includes
      %% Cmd.of_list (List.map Cmd.p files) )
  in

  let* () =
    match OS.Cmd.run clang_cmd with
    | Ok _ as v -> v
    | Error (`Msg e) ->
      Error
        (`Msg
           (Fmt.str "clang++ failed: %s"
              ( if debug then e
                else "run with --debug to get the full error message" ) ) )
  in

  let* llc_bin = OS.Cmd.resolve @@ Cmd.v "llc" in

  let files_bc =
    Cmd.of_list @@ List.map (fun file -> Cmd.p Fpath.(file -+ ".bc")) files
  in

  let llc_cmd : Cmd.t =
    Cmd.(
      llc_bin
      %
      (* TODO: configure this ? *)
      "-O0" % "-march=wasm32" % "-filetype=obj" %% files_bc )
  in

  let* () =
    match OS.Cmd.run llc_cmd with
    | Ok _ as v -> v
    | Error (`Msg e) ->
      Error
        (`Msg
           (Fmt.str "llc failed: %s"
              ( if debug then e
                else "run with --debug to get the full error message" ) ) )
  in
  let* wasmld_bin = OS.Cmd.resolve @@ Cmd.v "wasm-ld" in

  let files_o =
    Cmd.of_list @@ List.map (fun file -> Cmd.p Fpath.(file -+ ".o")) files
  in

  let out = Fpath.v "a.out.wasm" in

  let* binc = find binc_location (Fpath.v "libc.wasm") in
  let wasmld_cmd : Cmd.t =
    Cmd.(
      wasmld_bin % "-z" % "stack-size=8388608" % "--export=main"
      % "--entry=main" %% files_o % p binc % "-o" % p out )
  in

  let* () =
    match OS.Cmd.run wasmld_cmd with
    | Ok _ as v -> v
    | Error (`Msg e) ->
      Error
        (`Msg
           (Fmt.str "wasm-ld failed: %s"
              ( if debug then e
                else "run with --debug to get the full error message" ) ) )
  in

  Ok out

let cmd ~debug ~arch:_ ~workers ~opt_lvl ~includes ~files ~profiling ~unsafe
  ~optimize ~no_stop_at_failure ~no_value ~no_assert_failure_expression_printing
  ~deterministic_result_order ~fail_mode ~concolic ~solver : unit Result.t =
  let includes = libc_location @ includes in
  let* modul = compile ~includes ~opt_lvl debug files in
  let files = [ modul ] in
  (if concolic then Cmd_conc.cmd else Cmd_sym.cmd)
    ~profiling ~debug ~unsafe ~rac:false ~srac:false ~optimize ~workers
    ~no_stop_at_failure ~no_value ~no_assert_failure_expression_printing
    ~deterministic_result_order ~fail_mode ~workspace:(Fpath.v "owi-out")
    ~solver ~files
