(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

include
  Value_intf.T
    with type boolean = bool Smtml.Typed.t
     and type i32 = Smtml.Typed.bitv32 Smtml.Typed.t
     and type i64 = Smtml.Typed.bitv64 Smtml.Typed.t
     and type f32 = Smtml.Typed.float32 Smtml.Typed.t
     and type f64 = Smtml.Typed.float64 Smtml.Typed.t
     and type v128 = Smtml.Expr.t
     and module Ref = Symbolic_ref
