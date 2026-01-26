(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

include
  Value_intf.T
    with type boolean = Smtml.Typed.Bool.t
     and type i32 = Smtml.Typed.Bitv32.t
     and type i64 = Smtml.Typed.Bitv64.t
     and type f32 = Smtml.Typed.Float32.t
     and type f64 = Smtml.Typed.Float64.t
     and type v128 = Smtml.Typed.Bitv128.t
     and module Ref = Symbolic_ref
