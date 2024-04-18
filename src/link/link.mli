(*****************************************************************************)
(*                                                                           *)
(*  Owi                                                                      *)
(*                                                                           *)
(*  Copyright (C) 2021-2024 OCamlPro                                         *)
(*                                                                           *)
(*  SPDX-License-Identifier: AGPL-3.0-or-later                               *)
(*                                                                           *)
(*  This program is free software: you can redistribute it and/or modify     *)
(*  it under the terms of the GNU Affero General Public License as published *)
(*  by the Free Software Foundation, either version 3 of the License, or     *)
(*  (at your option) any later version.                                      *)
(*                                                                           *)
(*  This program is distributed in the hope that it will be useful,          *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of           *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            *)
(*  GNU Affero General Public License for more details.                      *)
(*                                                                           *)
(*  You should have received a copy of the GNU Affero General Public License *)
(*  along with this program.  If not, see <http://www.gnu.org/licenses/>.    *)
(*                                                                           *)
(*****************************************************************************)

(** Module to link a simplified/extern module and producing a runnable module
    along with a link state. *)

(** runtime env *)

open Types

(** runnable module *)
type 'f module_to_run =
  { modul : Simplified.modul
  ; env : 'f Link_env.t
  ; to_run : simplified expr list
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

type fenvs = Concrete_value.Func.extern_func Link_env.t Env_id.collection

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
  -> Simplified.modul
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
  -> func_typ:('f -> simplified func_type)
  -> 'f extern_module
  -> 'f state

val extern_module :
     Concrete_value.Func.extern_func state
  -> name:string
  -> Concrete_value.Func.extern_func extern_module
  -> Concrete_value.Func.extern_func state

type extern_func = Concrete_value.Func.extern_func Func_id.collection
