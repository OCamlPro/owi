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
  let open Concrete.Extern_func in
  let open Concrete.Extern_func.Syntax in
  let functions = [ ("assert", Extern_func (i32 ^->. unit, assert_i32)) ] in
  { functions }

let link_state = Link.extern_module Link.empty_state ~name:"owi" extern_module

let cmd ~unsafe ~timeout ~timeout_instr ~rac ~source_file =
  let name = None in
  let link_state = if rac then link_state else Link.empty_state in
  let* m, (link_state : _ Link.state) =
    Compile.File.until_link ~unsafe ~rac ~srac:false ~name link_state
      source_file
  in
  let+ () =
    Interpret.Concrete.modul ~timeout ~timeout_instr link_state.envs m
  in
  ()
