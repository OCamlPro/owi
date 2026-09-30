(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax

let run ~files ~out_file ~timeout ~timeout_instr ~unsafe ~workspace =
  let* source_file =
    Compile.Haskell.files_to_wasm_file ~files ~out_file ~workspace
  in
  Cmd_wasm_run.cmd ~source_file ~timeout ~timeout_instr ~unsafe

let sym ~entry_point ~files ~out_file ~symbolic_parameters ~workspace =
  let* source_file =
    Compile.Haskell.files_to_wasm_file ~files ~out_file ~workspace
  in

  Cmd_wasm_sym.cmd ~entry_point ~symbolic_parameters ~source_file ~workspace
