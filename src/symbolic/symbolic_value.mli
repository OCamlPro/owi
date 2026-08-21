(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

include
  Value_intf.T
    with type boolean = Symbolic_boolean.t
     and type i32 = Symbolic_i32.t
     and type i64 = Symbolic_i64.t
     and type f32 = Symbolic_f32.t
     and type f64 = Symbolic_f64.t
     and type v128 = Symbolic_v128.t
     and module Ref = Symbolic_ref
     and type Ref.i32 = Symbolic_i32.t

type context = unit

val of_concrete : context -> Concrete_value.t -> t
