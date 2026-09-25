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

  let* source_file =
    Compile.C.files_to_wasm_file ~eacsl ~entry_point ~includes ~opt_lvl
      ~out_file ~workspace files
  in
  let* () = Cmd_c.metadata ~workspace arch property files in
  (* TODO: use this! *)
  let _ = testcomp in
  (* TODO: use this! *)
  let _workspace = Some workspace in

  Cmd_wasm_fuzz.cmd ~entry_point ~rounds ~seed ~source_file ~timeout
    ~timeout_instr ~unsafe
