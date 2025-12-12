(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type t

include
  Memory_intf.T
    with type t := t
     and type i32 := Concrete_i32.t
     and type i64 := Concrete_i64.t
     and type 'a choice := 'a Concrete_choice.t

val get_limits : t -> Text.limits

val init : Text.limits -> t
