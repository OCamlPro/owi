(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax

let abs ~debug_trace ~entry_point ~files ~includes ~opt_lvl ~out_file ~unsafe =
  let* source_file =
    Compile.Rust.files_to_wasm_file ~entry_point ~files ~includes ~opt_lvl
      ~out_file
  in
  Cmd_wasm_abs.cmd ~debug_trace ~entry_point ~source_file ~unsafe

let fuzz ~entry_point ~files ~includes ~opt_lvl ~out_file ~rounds ~seed ~timeout
  ~timeout_instr ~unsafe =
  let* source_file =
    Compile.Rust.files_to_wasm_file ~entry_point ~files ~includes ~opt_lvl
      ~out_file
  in
  Cmd_wasm_fuzz.cmd ~entry_point ~rounds ~seed ~source_file ~timeout
    ~timeout_instr ~unsafe

let hunt ~entry_point ~files ~includes ~opt_lvl ~out_file ~rounds ~seed
  ~symbolic_parameters ~workspace =
  let* source_file =
    Compile.Rust.files_to_wasm_file ~entry_point ~files ~includes ~opt_lvl
      ~out_file
  in
  (* TODO: move these 3 out of symbolic param and put them in a dedicated monadic_interpreter_parameters ? *)
  let { Symbolic_parameters.timeout; timeout_instr; unsafe; _ } =
    symbolic_parameters
  in
  Cmd_wasm_hunt.cmd ~entry_point ~rounds ~seed ~source_file ~symbolic_parameters
    ~timeout ~timeout_instr ~unsafe ~workspace

let run ~entry_point ~files ~includes ~opt_lvl ~out_file ~timeout ~timeout_instr
  ~unsafe =
  let* source_file =
    Compile.Rust.files_to_wasm_file ~entry_point ~files ~includes ~opt_lvl
      ~out_file
  in
  Cmd_wasm_run.cmd ~source_file ~timeout ~timeout_instr ~unsafe

(* TODO: use arch *)
let sym ~arch:_ ~entry_point ~files ~includes ~opt_lvl ~out_file
  ~symbolic_parameters ~workspace =
  let* source_file =
    Compile.Rust.files_to_wasm_file ~entry_point ~files ~includes ~opt_lvl
      ~out_file
  in

  Cmd_wasm_sym.cmd ~entry_point ~source_file ~symbolic_parameters ~workspace
