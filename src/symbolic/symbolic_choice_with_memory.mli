(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

include
  Symbolic_choice_intf.S
    with type 'a t =
      ( 'a Symbolic_choice_intf.eval
      , Thread_with_memory.t )
      Symbolic_choice.CoreImpl.State.t
     and type thread := Thread_with_memory.t
     and module V := Symbolic_value

val lift_mem : 'a Symbolic_choice_without_memory.t -> 'a t
