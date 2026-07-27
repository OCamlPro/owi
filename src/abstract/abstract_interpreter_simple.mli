(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

type t =
  | State of Abstract_interpreter_state.t
  | Unreachable

val eval_instr :
     (   widens:bool
      -> Abstract_stack.Value.t
      -> Abstract_stack.Value.t
      -> Abstract_interpreter_state.t
      -> Abstract_interpreter_state.t
      -> ( Abstract_value.t
         , Abstract_domain.Context.empty_tuple )
         Abstract_domain.Context.result
      -> ('a -> 'b -> 'a)
      -> (Abstract_value.t, 'c) Abstract_domain.Context.result )
  -> Abstract_interpreter_state.t
  -> uuid:int
  -> Binary.simple_instruction
  -> t
