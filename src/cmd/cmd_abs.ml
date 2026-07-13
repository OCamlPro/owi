(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax

let link_state () =
  Link.State.empty ()
  |> Link.Extern.modul ~name:"owi" Abstract_wasm_ffi.symbolic_extern_module

let cmd ~source_file ~entry_point ~unsafe =
  let link_state = link_state () in

  let* m = Compile.File.until_binary ~unsafe source_file in
  let* m = Cmd_utils.set_entry_point entry_point false m in
  let+ m, link_state =
    Compile.Binary.until_link ~unsafe ~name:None link_state m
  in
  let state = Abstract_driver.modul link_state m in
  Abstract_checker.check_module link_state m state.invariant
