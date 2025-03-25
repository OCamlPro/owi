(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Bos
open Syntax

(* TODO: investigate which parameters makes sense *)
let compile ~entry_point ~includes:_ ~opt_lvl:_ debug (files : Fpath.t list) :
  Fpath.t Result.t =
  let entry_point = Option.value entry_point ~default:"main" in
  let* rustc_bin = OS.Cmd.resolve @@ Cmd.v "rustc" in

  let* libowi_sym_rlib =
    Cmd_utils.find_installed_rust_file (Fpath.v "libowi_sym.rlib")
  in

  let out = Fpath.v "a.out.wasm" in

  let rustc_cmd : Cmd.t =
    Cmd.(
      rustc_bin % "--target=wasm32-unknown-unknown" % "--edition=2021"
      % "--extern"
      % Fmt.str "owi_sym=%a" Fpath.pp libowi_sym_rlib
      % "-o" % Cmd.p out
      (* link args parameters must be space separated *)
      % "-C"
      % Fmt.str "link-args=--entry=%s" entry_point
      %% Cmd.of_list (List.map Cmd.p files) )
  in
  let+ () =
    match
      OS.Cmd.run
        ~err:(if debug then OS.Cmd.err_run_out else OS.Cmd.err_null)
        rustc_cmd
    with
    | Ok _ as v -> v
    | Error (`Msg e) ->
      Fmt.error_msg "rustc failed: %s"
        (if debug then e else "run with --debug to get the full error message")
  in

  out

let cmd ~debug ~arch:_ ~workers ~opt_lvl ~includes ~files ~profiling ~unsafe
  ~optimize ~no_stop_at_failure ~no_value ~no_assert_failure_expression_printing
  ~deterministic_result_order ~fail_mode ~concolic ~solver ~profile
  ~model_output_format ~entry_point ~invoke_with_symbols : unit Result.t =
  let* modul = compile ~entry_point ~includes ~opt_lvl debug files in
  let files = [ modul ] in
  (if concolic then Cmd_conc.cmd else Cmd_sym.cmd)
    ~profiling ~debug ~unsafe ~rac:false ~srac:false ~optimize ~workers
    ~no_stop_at_failure ~no_value ~no_assert_failure_expression_printing
    ~deterministic_result_order ~fail_mode ~workspace:(Fpath.v "owi-out")
    ~solver ~files ~profile ~model_output_format ~entry_point
    ~invoke_with_symbols
