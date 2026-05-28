(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax

let cmd ~source_file ~no_input ~unsafe =
  let link_state = Link.State.empty () in
  let+ m, link_state =
    Compile.File.until_link ~unsafe ~name:None link_state source_file
  in
  Denot_interpreter.run ~no_input link_state m
