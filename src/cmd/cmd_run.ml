(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax

let extern_module : Concrete_extern_func.extern_func Link.extern_module =
  let assert_i32 n =
    (* TODO: proper Error here ? *)
    assert (not @@ Prelude.Int32.equal n 0l);
    Ok ()
  in
  let functions =
    [ ( "assert"
      , Concrete_extern_func.Extern_func (Func (Arg (I32, Res), R0), assert_i32)
      )
    ]
  in
  { functions }

(* module name is called "symbolic" to be compatible with code generator *)
let link_state =
  Link.extern_module Link.empty_state ~name:"symbolic" extern_module

let run_file ~unsafe ~rac ~optimize filename =
  let name = None in
  let link_state = if rac then link_state else Link.empty_state in
  let+ (_ : _ Link.state) =
    Compile.File.until_interpret ~unsafe ~rac ~srac:false ~optimize ~name
      link_state filename
  in
  ()

let cmd ~unsafe ~rac ~optimize ~files =
  list_iter (run_file ~unsafe ~rac ~optimize) files
