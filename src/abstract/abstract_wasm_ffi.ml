(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

open Abstract_extern_func
open Abstract_extern_func.Syntax

(* The constraint is used here to make sure we don't forget to define one of
   the expected FFI functions, this whole file is further constrained such that
   if one function of M is unused in the FFI module below, an error will be
   displayed *)
let symbol_i32 () = raise (ExternFuncException I32_symbol)

let symbol_i64 () = raise (ExternFuncException I64_symbol)

let assume () = raise (ExternFuncException Assume)

let symbolic_extern_module =
  let functions =
    [ ("i32_symbol", Extern_func (unit ^->. i32, symbol_i32))
    ; ("i64_symbol", Extern_func (unit ^->. i64, symbol_i64))
    ; ("assume", Extern_func (unit ^-> i32 ^->. unit, assume))
      (* ; ("assert", Extern_func (i32 ^->. unit, assert')) *)
      (* ; ("exit", Extern_func (i32 ^->. unit, exit)) *)
    ]
  in
  { Extern.Module.functions; func_type = Abstract_extern_func.extern_type }
