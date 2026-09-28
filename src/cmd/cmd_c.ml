(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax

(* TODO: use testcomp! *)
let fuzz ~arch ~eacsl ~entry_point ~files ~includes ~opt_lvl ~out_file ~property
  ~rounds ~seed ~testcomp:_ ~timeout ~timeout_instr ~unsafe ~workspace :
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

let hunt ~arch ~eacsl ~entry_point ~files ~includes ~opt_lvl ~out_file ~property
  ~rounds ~seed ~(symbolic_parameters : Symbolic_parameters.t) : unit Result.t =
  let* workspace =
    Cmd_utils.make_workspace ~workspace:symbolic_parameters.workspace
  in

  let* source_file =
    Compile.C.files_to_wasm_file ~arch ~eacsl ~entry_point ~includes ~opt_lvl
      ~out_file ~property ~workspace files
  in
  let workspace = Some workspace in

  let symbolic_parameters = { symbolic_parameters with workspace } in
  let { Symbolic_parameters.timeout; timeout_instr; unsafe; _ } =
    symbolic_parameters
  in

  let* () =
    Cmd_wasm_fuzz.cmd ~entry_point ~rounds ~seed ~source_file ~timeout
      ~timeout_instr ~unsafe
  in

  Cmd_wasm_sym.cmd ~entry_point ~source_file ~symbolic_parameters

(* TODO: use testcomp *)
let sym ~arch ~eacsl ~entry_point ~files ~includes ~opt_lvl ~out_file ~property
  ~(symbolic_parameters : Symbolic_parameters.t) ~testcomp:_ : unit Result.t =
  let* workspace =
    Cmd_utils.make_workspace ~workspace:symbolic_parameters.workspace
  in

  let* source_file =
    Compile.C.files_to_wasm_file ~arch ~eacsl ~entry_point ~includes ~opt_lvl
      ~out_file ~property ~workspace files
  in
  let workspace = Some workspace in

  let symbolic_parameters = { symbolic_parameters with workspace } in

  Cmd_wasm_sym.cmd ~entry_point ~source_file ~symbolic_parameters
