(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Bos
open Syntax

(* TODO: investigate which parameters makes sense *)
let compile ~entry_point ~includes:_ ~opt_lvl:_ ~out_file (files : Fpath.t list)
  : Fpath.t Result.t =
  let* rustc_bin = OS.Cmd.resolve @@ Cmd.v "rustc" in

  let* libowi_sym_rlib =
    Cmd_utils.find_installed_rust_file (Fpath.v "libowi_sym.rlib")
  in

  let* tmp = Bos.OS.Dir.tmp "owi_rust_%s" in
  let out = Option.value ~default:Fpath.(tmp / "a.out.wasm") out_file in

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

  let err =
    match Logs.level () with
    | Some (Logs.Debug | Logs.Info) -> OS.Cmd.err_run_out
    | None | Some _ -> OS.Cmd.err_null
  in

  let+ () =
    match OS.Cmd.run ~err rustc_cmd with
    | Ok _ as v -> v
    | Error (`Msg e) ->
      Logs.debug (fun m -> m "rustc failed: %s" e);
      Fmt.error_msg
        "rustc failed: run with -vv to get the full error message if it was \
         not displayed above"
  in

  out

let cmd ~arch:_ ~workers ~opt_lvl ~includes ~files ~unsafe ~optimize
  ~no_stop_at_failure ~no_value ~no_assert_failure_expression_printing
  ~deterministic_result_order ~fail_mode ~concolic ~solver ~model_format
  ~entry_point ~invoke_with_symbols ~out_file ~(workspace : Fpath.t option)
  ~model_out_file ~with_breadcrumbs : unit Result.t =
  let* workspace =
    match workspace with
    | Some path -> Ok path
    | None -> OS.Dir.tmp "owi_rust_%s"
  in
  let* _did_create : bool = OS.Dir.create workspace in
  let entry_point = Option.value entry_point ~default:"main" in

  let* modul = compile ~entry_point ~includes ~opt_lvl ~out_file files in
  let files = [ modul ] in
  let entry_point = Some entry_point in
  let workspace = Some workspace in
  (if concolic then Cmd_conc.cmd else Cmd_sym.cmd)
    ~unsafe ~rac:false ~srac:false ~optimize ~workers ~no_stop_at_failure
    ~no_value ~no_assert_failure_expression_printing ~deterministic_result_order
    ~fail_mode ~workspace ~solver ~files ~model_format ~entry_point
    ~invoke_with_symbols ~model_out_file ~with_breadcrumbs
