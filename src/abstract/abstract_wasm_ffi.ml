(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

(* TODO: do this when we have the monad *)
module M = struct
  let symbol_i32 () = assert false
end

open M
open Abstract_extern_func
open Abstract_extern_func.Syntax

let symbolic_extern_module =
  let functions =
    [ ("i32_symbol", Extern_func (unit ^->. i32, symbol_i32))
      (* ; ("assume", Extern_func (i32 ^->. unit, assume)) *)
      (* ; ("assert", Extern_func (i32 ^->. unit, assert')) *)
      (* ; ("exit", Extern_func (i32 ^->. unit, exit)) *)
    ]
  in
  { Extern.Module.functions; func_type = Abstract_extern_func.extern_type }
