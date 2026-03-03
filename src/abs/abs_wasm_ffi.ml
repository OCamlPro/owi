(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

(* The constraint is used here to make sure we don't forget to define one of
   the expected FFI functions, this whole file is further constrained such that
   if one function of M is unused in the FFI module below, an error will be
   displayed *)
module M = struct
  module Value = Abs_value.SVA

  let assume (b : Symbolic_i32.t) : unit Symbolic_choice.t =
    Symbolic_choice.assume (Symbolic_i32.to_boolean b)

  let assert' (b : Symbolic_i32.t) : unit Symbolic_choice.t =
    Symbolic_choice.assertion @@ Symbolic_i32.to_boolean b

  let symbol_i32 () =
    Ok (Value.Bitvector_Lattice.top ~size:(Units.In_bits.of_int 32))

  let abort () : unit Symbolic_choice.t = Symbolic_choice.prune ()

  let exit (_p : Symbolic_i32.t) : unit Symbolic_choice.t = abort ()
end

open M
include Abs_extern_func
include Abs_extern_func.Syntax

let symbolic_extern_module =
  let functions =
    [ ("i32_symbol", Extern_func (unit ^->. i32, symbol_i32))
    (* ; ("assume", Extern_func (i32 ^->. unit, assume)) *)
    (* ; ("assert", Extern_func (i32 ^->. unit, assert')) *)
    (* ; ("exit", Extern_func (i32 ^->. unit, exit)) *)
    ]
  in
  { Extern.Module.functions; func_type = Abs_extern_func.extern_type }
