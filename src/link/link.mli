(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

(** Module to link a binary/extern module and producing a runnable module along
    with a link state. *)

(** runtime env *)

open Types

(** runnable module *)
type 'f module_to_run =
  { id : string option
  ; env : 'f Link_env.t
  ; to_run : binary expr Annotated.t list
  }

module StringMap : Map.S with type key = string

module StringSet : Set.S

type func := Func_intf.t

(** runtime exported items *)
type exports =
  { globals : Concrete_global.t StringMap.t
  ; memories : Concrete_memory.t StringMap.t
  ; tables : Concrete_table.t StringMap.t
  ; functions : func StringMap.t
  ; defined_names : StringSet.t
  }

type 'ext envs = 'ext Link_env.t Env_id.collection

type fenvs = Concrete_extern_func.extern_func Link_env.t Env_id.collection

(** link state *)
type 'f state =
  { by_name : exports StringMap.t
  ; by_id : (exports * Env_id.t) StringMap.t
  ; last : (exports * Env_id.t) option
  ; collection : 'f Func_id.collection
  ; envs : 'f envs
  }

(** the empty link state *)
val empty_state : 'f state

(** link a module with a given link state, producing a runnable module and a new
    link state *)
val modul :
     'f state
  -> name:string option
  -> Binary.Module.t
  -> ('f module_to_run * 'f state) Result.t

(** register a module inside a link state, producing a new link state *)
val register_module :
  'f state -> name:string -> id:string option -> 'f state Result.t

(** extern modules *)
type 'extern_func extern_module = { functions : (string * 'extern_func) list }

(** register an extern module with a given link state, producing a new link
    state *)
val extern_module' :
     'f state
  -> name:string
  -> func_typ:('f -> binary func_type)
  -> 'f extern_module
  -> 'f state

val extern_module :
     Concrete_extern_func.extern_func state
  -> name:string
  -> Concrete_extern_func.extern_func extern_module
  -> Concrete_extern_func.extern_func state

type extern_func = Concrete_extern_func.extern_func Func_id.collection
