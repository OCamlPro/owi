(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax

let sym ~entry_point ~files ~out_file ~symbolic_parameters ~workspace :
  unit Result.t =
  let* source_file =
    Compile.Llvm.files_to_wasm_file ~entry_point ~files ~out_file ~workspace
  in

  Cmd_wasm_sym.cmd ~entry_point ~source_file ~symbolic_parameters ~workspace
