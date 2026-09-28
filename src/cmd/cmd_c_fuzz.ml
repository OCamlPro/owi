(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax

(* TODO: use testcomp! *)
let cmd ~rounds ~seed ~workspace ~entry_point ~arch ~property ~testcomp:_
  ~opt_lvl ~includes ~files ~eacsl ~out_file ~timeout ~timeout_instr ~unsafe :
  unit Result.t =
  let* workspace = Cmd_utils.make_workspace ~workspace in

  let* source_file =
    Compile.C.files_to_wasm_file ~arch ~eacsl ~entry_point ~includes ~opt_lvl
      ~out_file ~property ~workspace files
  in
  (* TODO: use this! *)
  let _workspace = Some workspace in

  Cmd_wasm_fuzz.cmd ~entry_point ~rounds ~seed ~source_file ~timeout
    ~timeout_instr ~unsafe
