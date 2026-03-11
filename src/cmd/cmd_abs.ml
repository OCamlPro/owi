(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax

let cmd ~source_file =
  let link_state =
    Link.State.empty ()
    |> Link.Extern.modul ~name:"owi" Abs_wasm_ffi.symbolic_extern_module
  in
  let+ m, link_state =
    Compile.File.until_link ~unsafe:true ~name:None link_state source_file
  in
  (* let start = match m.start with Some i -> i | None -> assert false in *)
  (* let f = m.func.(start) in *)
  (* let expr = match f with Origin.Local f -> f.body | _ -> assert false in *)
  Abs_driver.expr link_state m
