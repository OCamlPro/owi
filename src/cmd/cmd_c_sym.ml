(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

open Bos
open Syntax

(* TODO: use testcomp *)
let cmd ~arch ~eacsl ~entry_point ~files ~includes ~opt_lvl ~out_file ~property
  ~(symbolic_parameters : Symbolic_parameters.t) ~testcomp:_ : unit Result.t =
  let* workspace =
    match symbolic_parameters.workspace with
    | Some path -> Ok path
    | None -> Bos.OS.Dir.tmp "owi_c_%s"
  in
  let* _did_create : bool = OS.Dir.create Fpath.(workspace / "test-suite") in

  let includes = Cmd_utils.c_files_location @ includes in
  let* files = Cmd_c.eacsl_instrument eacsl ~includes files in
  let* source_file =
    Cmd_c.compile ~workspace ~entry_point ~includes ~opt_lvl ~out_file files
  in
  let* () = Cmd_c.metadata ~workspace arch property files in
  let workspace = Some workspace in

  let symbolic_parameters = { symbolic_parameters with workspace } in

  Cmd_wasm_sym.cmd ~entry_point ~source_file ~symbolic_parameters
