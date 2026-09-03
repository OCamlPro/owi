(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax

let cmd ~rounds ~seed ~workspace ~entry_point ~arch ~property ~testcomp ~opt_lvl
  ~includes ~files ~eacsl ~out_file ~timeout ~timeout_instr ~unsafe :
  unit Result.t =
  let* workspace =
    match workspace with
    | Some path -> Ok path
    | None -> Bos.OS.Dir.tmp "owi_c_fuzz_%s"
  in
  let* _did_create : bool =
    Bos.OS.Dir.create Fpath.(workspace / "test-suite")
  in

  let includes = Cmd_utils.c_files_location @ includes in
  let* files = Cmd_c.eacsl_instrument eacsl ~includes files in
  let* source_file =
    Cmd_c.compile ~workspace ~entry_point ~includes ~opt_lvl ~out_file files
  in
  let* () = Cmd_c.metadata ~workspace arch property files in
  (* TODO: use this! *)
  let _ = testcomp in
  (* TODO: use this! *)
  let _workspace = Some workspace in

  Cmd_wasm_fuzz.cmd ~entry_point ~rounds ~seed ~source_file ~timeout
    ~timeout_instr ~unsafe
