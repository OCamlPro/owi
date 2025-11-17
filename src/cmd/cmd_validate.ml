(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax

let validate filename =
  let+ (_modul : Binary.Module.t) =
    Compile.File.until_validate ~unsafe:false filename
  in
  ()

let cmd ~files = list_iter validate files
