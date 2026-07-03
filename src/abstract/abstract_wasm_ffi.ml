(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

open Abstract_monad

(* The constraint is used here to make sure we don't forget to define one of
   the expected FFI functions, this whole file is further constrained such that
   if one function of M is unused in the FFI module below, an error will be
   displayed *)
let symbol_i32 () = fold_state (fun { ctx; _ } -> Abstract_i32.unknown ctx)

let symbol_i64 () = fold_state (fun { ctx; _ } -> Abstract_i64.unknown ctx)

let assume condition =
  map_state (fun ({ ctx; _ } as state) ->
    let condition = Abstract_i32.to_boolean ctx condition in
    match Abstract_domain.assume ctx condition with
    | None -> None
    | Some ctx -> Some { state with ctx } )

open Abstract_extern_func
open Abstract_extern_func.Syntax

let symbolic_extern_module =
  let functions =
    [ ("i32_symbol", Extern_func (unit ^->. i32, symbol_i32))
    ; ("i64_symbol", Extern_func (unit ^->. i64, symbol_i64))
    ; ("assume", Extern_func (i32 ^->. unit, assume))
      (* ; ("assert", Extern_func (i32 ^->. unit, assert')) *)
      (* ; ("exit", Extern_func (i32 ^->. unit, exit)) *)
    ]
  in
  { Extern.Module.functions; func_type = Abstract_extern_func.extern_type }
