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
  let flags =
    let stack_size = 8 * 1024 * 1024 |> string_of_int in
    let includes = Cmd.of_list ~slip:"-I" (List.map Fpath.to_string includes) in
    Cmd.(
      of_list
        [ Fmt.str "-O%s" opt_lvl
        ; "--target=wasm32"
        ; "-m32"
        ; "-ffreestanding"
        ; "--no-standard-libraries"
        ; "-Wno-everything"
        ; "-flto=thin"
        ; (* LINKER FLAGS: *)
          "-Wl,--entry=main"
        ; "-Wl,--export=main"
          (* TODO: allow this behind a flag, this is slooooow *)
        ; "-Wl,--lto-O0"
        ; Fmt.str "-Wl,-z,stack-size=%s" stack_size
        ]
      %% includes )
  in

  let* clangpp_bin = OS.Cmd.resolve @@ Cmd.v "clang++" in

  let out = Fpath.(v "a.out.wasm") in

  let* libc = find binc_location (Fpath.v "libc.wasm") in

  let files = Cmd.of_list (List.map Fpath.to_string (libc :: files)) in
  let clangpp : Cmd.t = Cmd.(clangpp_bin %% flags % "-o" % p out %% files) in

  let+ () =
    match OS.Cmd.run clangpp with
    | Ok _ as v -> v
    | Error (`Msg e) ->
      Error
        (`Msg
           (Fmt.str "clang++ failed: %s"
              ( if debug then e
                else "run with --debug to get the full error message" ) ) )
  in

  out

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
