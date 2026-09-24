(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax

let cmd ~rounds ~seed ~source_file ~symbolic_parameters ~timeout ~timeout_instr
  ~unsafe =
  let { Symbolic_parameters.entry_point; _ } = symbolic_parameters in
  let* () =
    Cmd_wasm_fuzz.cmd ~entry_point ~rounds ~seed ~source_file ~timeout
      ~timeout_instr ~unsafe
  in
  Cmd_wasm_sym.cmd ~parameters:symbolic_parameters ~source_file
