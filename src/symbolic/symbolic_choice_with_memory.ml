(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

include Symbolic_choice.Make (Thread_with_memory)

let lift_mem (mem_op : 'a Symbolic_choice_without_memory.t) : 'a t =
  Symbolic_choice.CoreImpl.State.project_state Thread_with_memory.project
    Thread_with_memory.restore mem_op
