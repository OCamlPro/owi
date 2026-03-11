(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

(* The constraint is used here to make sure we don't forget to define one of
   the expected FFI functions, this whole file is further constrained such that
   if one function of M is unused in the FFI module below, an error will be
   displayed *)
module M = struct
  (* let symbol_i32 () = *)
  (*   Ok *)
  (*     (Abs_value.ADomain.binary_unknown ~size:Units.In_bits.s32 *)
  (*        (Abs_value.ADomain.root_context ()) ) *)
end

(* open M *)
(* open Abs_extern_func *)
(* open Abs_extern_func.Syntax *)

let symbolic_extern_module =
  let functions =
    [ (* ("i32_symbol", Extern_func (unit ^->. i32, symbol_i32)) *)
      (* ; ("assume", Extern_func (i32 ^->. unit, assume)) *)
      (* ; ("assert", Extern_func (i32 ^->. unit, assert')) *)
      (* ; ("exit", Extern_func (i32 ^->. unit, exit)) *)
    ]
  in
  { Extern.Module.functions; func_type = Abs_extern_func.extern_type }
