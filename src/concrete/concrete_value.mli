(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

include
  Value_intf.T
    with type boolean = Concrete_boolean.t
     and type i32 = Concrete_i32.t
     and type i64 = Concrete_i64.t
     and type f32 = Concrete_f32.t
     and type f64 = Concrete_f64.t
     and type v128 = Concrete_v128.t
     and module Ref = Concrete_ref
