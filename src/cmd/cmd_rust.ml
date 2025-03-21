(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Bos
open Syntax

(* TODO: refactor to re-use code in Cmd_c.ml *)
let rust_files = List.map Fpath.v Share.Sites.rust_files

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

(* TODO: investigate which parameters makes sense *)
let compile ~includes:_ ~opt_lvl:_ debug (files : Fpath.t list) :
  Fpath.t Result.t =
  let* rustc_bin = OS.Cmd.resolve @@ Cmd.v "rustc" in

  let* libowi_sym_rlib = find rust_files (Fpath.v "libowi_sym.rlib") in

  let out = Fpath.v "a.out.wasm" in

  let rustc_cmd : Cmd.t =
    Cmd.(
      rustc_bin % "--target=wasm32-unknown-unknown" % "--edition=2021"
      % "--extern"
      % Fmt.str "owi_sym=%a" Fpath.pp libowi_sym_rlib
      % "-o" % Cmd.p out
      %% Cmd.of_list (List.map Cmd.p files) )
  in
  let+ () =
    match OS.Cmd.run rustc_cmd with
    | Ok _ as v -> v
    | Error (`Msg e) ->
      Fmt.error_msg "rustc failed: %s"
        (if debug then e else "run with --debug to get the full error message")
  in

  out

let cmd ~debug ~arch:_ ~workers ~opt_lvl ~includes ~files ~profiling ~unsafe
  ~optimize ~no_stop_at_failure ~no_value ~no_assert_failure_expression_printing
  ~deterministic_result_order ~fail_mode ~concolic ~solver ~profile
  ~model_output_format ~entry_point : unit Result.t =
  let* modul = compile ~includes ~opt_lvl debug files in
  let files = [ modul ] in
  (if concolic then Cmd_conc.cmd else Cmd_sym.cmd)
    ~profiling ~debug ~unsafe ~rac:false ~srac:false ~optimize ~workers
    ~no_stop_at_failure ~no_value ~no_assert_failure_expression_printing
    ~deterministic_result_order ~fail_mode ~workspace:(Fpath.v "owi-out")
    ~solver ~files ~profile ~model_output_format ~entry_point
