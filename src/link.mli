(** Module to link a simplified/extern module and producing a runnable module
    along with a link state. *)

(** runtime env *)
module Env = Link_env

(** runnable module *)
type module_to_run =
  { modul : Simplified.modul
  ; env : Env.t
  ; to_run : Simplified.expr list
  }

module StringMap : Map.S with type key = string

module StringSet : Set.S

(** runtime exported items *)
type exports =
  { globals : Env.t' Global.t StringMap.t
  ; memories : Memory.t StringMap.t
  ; tables : Env.t' Table.t StringMap.t
  ; functions : Env.t' Value.Func.t StringMap.t
  ; defined_names : StringSet.t
  }

(** link state *)
type state =
  { by_name : exports StringMap.t
  ; by_id : exports StringMap.t
  ; last : exports option
  }

(** the empty link state *)
val empty_state : state

(** link a module with a given link state, producing a runnable module and a new
    link state *)
val modul :
     state
  -> name:string option
  -> Simplified.modul
  -> (module_to_run * state) Result.t

(** register a module inside a link state, producing a new link state *)
val register_module : state -> name:string -> id:string option -> state Result.t

(** extern modules *)
type extern_module = { functions : (string * Value.Func.extern_func) list }

(** register an extern module with a given link state, producing a new link
    state *)
val extern_module : state -> name:string -> extern_module -> state
