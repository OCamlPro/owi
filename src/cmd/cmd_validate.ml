(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax

let validate rac filename =
  let+ (_modul : Binary.modul) =
<<<<<<< HEAD
    Compile.File.until_binary_validate ~unsafe:false filename
=======
    Compile.File.until_typecheck ~unsafe:false ~rac filename
>>>>>>> 66d816d7 (framework for rac code generation)
  in
  ()

let cmd debug rac files =
  if debug then Log.debug_on := true;
  list_iter (validate rac) files
