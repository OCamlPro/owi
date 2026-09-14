(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

type kind =
  | Block_start
  | Block_end
  | Step
  | Join
  | Widen

val enable : unit -> unit

val is_enabled : unit -> bool

val record_step :
     ?inputs:(string * Abstract_interpreter_state.t option) list
  -> ?converged:bool option
  -> kind
  -> Abstract_interpreter_state.t option
  -> Binary.instr Annotated.t
  -> unit

val record_jt : jt:Abstract_jump_map.t -> unit

val record_warning : instr_id:int -> message:string -> unit

val write_json : string -> unit
