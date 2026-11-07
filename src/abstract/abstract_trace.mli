(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

type kind =
  | Block_start
  | Block_end
  | Step
  | Join
  | Widen

type test_res =
  | Ok
  | Fail of
      { expected : string
      ; got : string
      }

val enable : string -> unit

val close : unit -> unit

val is_enabled : unit -> bool

val trace_wasm_step :
     ?inputs:(string * Abstract_interpreter_state.t option) list
  -> ?converged:bool option
  -> kind
  -> Abstract_interpreter_state.t option
  -> Binary.instr Annotated.t
  -> unit

val trace_wasm_jt : jm:Abstract_jump_map.t -> unit

val trace_wasm_warning : instr_id:int -> message:string -> unit

val trace_wast_cmd : kind -> Wast.cmd -> unit

val trace_wast_test_res : test_res -> unit
