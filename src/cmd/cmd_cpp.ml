(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax

let run ~entry_point ~files ~includes ~opt_lvl ~out_file ~timeout ~timeout_instr
  ~unsafe ~workspace =
  let* source_file =
    Compile.Cpp.files_to_wasm_file ~entry_point ~files ~includes ~opt_lvl
      ~out_file ~workspace
  in
  Cmd_wasm_run.cmd ~source_file ~timeout ~timeout_instr ~unsafe

(* TODO: use arch *)
let sym ~arch:_ ~entry_point ~files ~includes ~opt_lvl ~out_file
  ~symbolic_parameters ~workspace : unit Result.t =
  let* source_file =
    Compile.Cpp.files_to_wasm_file ~entry_point ~files ~includes ~opt_lvl
      ~out_file ~workspace
  in

  Cmd_wasm_sym.cmd ~entry_point ~source_file ~symbolic_parameters ~workspace
