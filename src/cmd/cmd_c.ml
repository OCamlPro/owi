(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax

(* TODO: use testcomp! *)
let fuzz ~arch ~eacsl ~entry_point ~files ~includes ~opt_lvl ~out_file ~property
  ~rounds ~seed ~testcomp:_ ~timeout ~timeout_instr ~unsafe ~workspace :
  unit Result.t =
  let* source_file =
    Compile.C.files_to_wasm_file ~arch ~eacsl ~entry_point ~includes ~opt_lvl
      ~out_file ~property ~workspace files
  in

  Cmd_wasm_fuzz.cmd ~entry_point ~rounds ~seed ~source_file ~timeout
    ~timeout_instr ~unsafe

let hunt ~arch ~eacsl ~entry_point ~files ~includes ~opt_lvl ~out_file ~property
  ~rounds ~seed ~(symbolic_parameters : Symbolic_parameters.t) ~workspace :
  unit Result.t =
  let* source_file =
    Compile.C.files_to_wasm_file ~arch ~eacsl ~entry_point ~includes ~opt_lvl
      ~out_file ~property ~workspace files
  in

  (* TODO: move these 3 out of symbolic param and put them in a dedicated monadic_interpreter_parameters ? *)
  let { Symbolic_parameters.timeout; timeout_instr; unsafe; _ } =
    symbolic_parameters
  in

  Cmd_wasm_hunt.cmd ~entry_point ~rounds ~seed ~source_file ~symbolic_parameters
    ~timeout ~timeout_instr ~unsafe ~workspace

(* TODO: use testcomp *)
let sym ~arch ~eacsl ~entry_point ~files ~includes ~opt_lvl ~out_file ~property
  ~(symbolic_parameters : Symbolic_parameters.t) ~testcomp:_ ~workspace :
  unit Result.t =
  let* source_file =
    Compile.C.files_to_wasm_file ~arch ~eacsl ~entry_point ~includes ~opt_lvl
      ~out_file ~property ~workspace files
  in

  Cmd_wasm_sym.cmd ~entry_point ~source_file ~symbolic_parameters ~workspace
