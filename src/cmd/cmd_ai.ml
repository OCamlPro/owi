(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax

let cmd ~source_file =
  let* m = Compile.File.until_binary ~unsafe:false source_file in
  let start = match m.start with Some i -> i | None -> assert false in
  let f = m.func.(start) in
  let expr = match f with Origin.Local f -> f.body | _ -> assert false in
  Ok (Int_abs.expr expr)
