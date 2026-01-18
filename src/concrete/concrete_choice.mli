(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

(* TODO: type 'a t should be abstract, run will be needed for this *)
include
  Choice_intf.S
    with type 'a t = 'a Result.t
     and type boolean := Concrete_boolean.t
     and type i32 := Concrete_i32.t
     and type value := Concrete_value.t

val run : 'a t -> 'a t
