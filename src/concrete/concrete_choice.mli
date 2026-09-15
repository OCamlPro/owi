(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

include
  Choice_intf.S
    with type boolean := Concrete_boolean.t
     and type i32 := Concrete_i32.t
     and type value := Concrete_value.t

val run : 'a t -> 'a Result.t
