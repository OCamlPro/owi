(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

type kind =
  | Block_start
  | Block_end
  | Step
  | Join
  | Widen

type state_trace =
  { stack : string list
  ; locals : string list
  ; call_stack : int list
  ; globals : string list
  }

type jump_target =
  { label : string
  ; states : state_trace list
  }

type named_state =
  { name : string
  ; state : state_trace option
  }

type event =
  { id : int
  ; instr_id : int
  ; instr : string
  ; state_trace : state_trace option
  ; kind : kind
  ; jts : jump_target list option
  ; inputs : named_state list option
  ; converged : bool option
  ; warnings : string list
  }

val enable : unit -> unit

val is_enabled : unit -> bool

val reset : unit -> unit

val record_step :
     kind:kind
  -> instr:Binary.instr Annotated.t
  -> inputs:(string * Abstract_state.t option) list option
  -> converged:bool option
  -> state:Abstract_state.t option
  -> unit

val record_jt : jt:Abstract_jump_map.t -> unit

val record_warning : instr_id:int -> message:string -> unit

val write_json : string -> unit
