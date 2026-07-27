(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

type t

include
  Memory_intf.T
    with type t := t
     and type i32 := Symbolic_i32.t
     and type i64 := Symbolic_i64.t
     and type v128 := Symbolic_v128.t
     and type 'a choice := 'a Symbolic_choice.t
