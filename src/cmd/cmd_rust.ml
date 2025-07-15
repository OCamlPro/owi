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
      % ( match entry_point with
        | Some entry_point -> Fmt.str "link-args=--entry=%s" entry_point
        | None -> (* TODO: meh *) "" )
      %% Cmd.of_list (List.map Cmd.p files) )
  in

  let err =
    match Logs.level () with
    | Some (Logs.Debug | Logs.Info) -> OS.Cmd.err_run_out
    | None | Some _ -> OS.Cmd.err_null
  in

  (* TODO: does not seem to work, once it does, we can remove the `#![no_main]` in many documentation examples
  let* () = Bos.OS.Env.set_var "RUSTFLAGS" (Some "-Zcrate-attr=no_main") in
  *)
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

let cmd ~symbolic_parameters ~arch:_ ~opt_lvl ~includes ~files ~concolic
  ~out_file : unit Result.t =
  let* workspace =
    match symbolic_parameters.Cmd_sym.workspace with
    | Some path -> Ok path
    | None -> OS.Dir.tmp "owi_rust_%s"
  in
  let* _did_create : bool = OS.Dir.create workspace in

  let* source_file =
    compile ~entry_point:symbolic_parameters.entry_point ~includes ~opt_lvl
      ~out_file files
  in
  let workspace = Some workspace in

  let parameters = { symbolic_parameters with workspace } in

  (if concolic then Cmd_conc.cmd else Cmd_sym.cmd) ~parameters ~source_file
