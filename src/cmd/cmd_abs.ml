(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax

let link_state () =
  Link.State.empty ()
  |> Link.Extern.modul ~name:"owi" Abstract_wasm_ffi.symbolic_extern_module

let cmd ~source_file ~unsafe =
  let link_state = link_state () in
  let+ m, link_state =
    Compile.File.until_link ~unsafe ~name:None link_state source_file
  in
  let state = Abstract_driver.expr link_state m in
  Abstract_checker.check_module link_state m state.invariant

let from_binary m ~unsafe =
  let link_state = link_state () in
  let+ m, link_state =
    Compile.Binary.until_link ~unsafe ~name:None link_state m
  in
  Abstract_driver.expr link_state m
