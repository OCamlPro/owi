(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

include
  Symbolic_choice_intf.S
    with type 'a t = (('a, Bug.t) result, Thread_with_memory.t) State_monad.t
     and type thread := Thread_with_memory.t

val lift_mem : 'a Symbolic_choice_without_memory.t -> 'a t
