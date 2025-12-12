(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

include
  Value_intf.T
    with type boolean = Concrete_boolean.t
     and type i32 = Int32.t
     and type i64 = Int64.t
     and type f32 = Float32.t
     and type f64 = Float64.t
     and type v128 = Concrete_v128.t
     and module Ref = Concrete_ref
