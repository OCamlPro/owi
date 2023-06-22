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

type func := Func_intf.t

(** runtime exported items *)
type exports =
  { globals : Global.t StringMap.t
  ; memories : Memory.t StringMap.t
  ; tables : Table.t StringMap.t
  ; functions : func StringMap.t
  ; defined_names : StringSet.t
  }

type envs = Env.t Env_id.collection

(** link state *)
type 'f state =
  { by_name : exports StringMap.t
  ; by_id : exports StringMap.t
  ; last : exports option
  ; collection : 'f Func_id.collection
  ; envs : envs
  }

(** the empty link state *)
val empty_state : 'f state

(** link a module with a given link state, producing a runnable module and a new
    link state *)
val modul :
     Value.Func.extern_func state
  -> name:string option
  -> Simplified.modul
  -> (module_to_run * Value.Func.extern_func state) Result.t

(** register a module inside a link state, producing a new link state *)
val register_module :
  'f state -> name:string -> id:string option -> 'f state Result.t

(** extern modules *)
type 'extern_func extern_module = { functions : (string * 'extern_func) list }

(** register an extern module with a given link state, producing a new link
    state *)
val extern_module :
     'f state
  -> name:string
  -> func_typ:('f -> Simplified.func_type)
  -> 'f extern_module
  -> 'f state

type extern_func = Value.Func.extern_func Func_id.collection
