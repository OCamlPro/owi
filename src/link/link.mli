(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

(** Module to link a binary/extern module and producing a runnable module along
    with a link state. *)

(** runtime env *)

module StringMap : Map.S with type key = string

module StringSet : Set.S

module State : sig
  (** runtime exported items *)
  type exports =
    { globals : Concrete_global.t StringMap.t
    ; memories : Concrete_memory.t StringMap.t
    ; tables : Concrete_table.t StringMap.t
    ; functions : Kind.func StringMap.t
    ; tags : Binary.Tag.t StringMap.t
    ; defined_names : StringSet.t
    }

  type 'ext envs = 'ext Link_env.t Dynarray.t

  type 'f t

  (** the empty link state *)
  val empty : unit -> 'f t

  val get_envs : 'f t -> 'f envs

  val get_global_from_module :
    _ t -> string option -> string -> Concrete_global.t Result.t

  val get_func_from_module :
    _ t -> string option -> string -> (Kind.func * int) Result.t
end

module Extern : sig
  (** register an extern module with a given link state, producing a new link
      state *)
  val concrete_module :
       name:string
    -> Concrete_extern.Module.t
    -> Concrete_extern.Func.t State.t
    -> Concrete_extern.Func.t State.t

  val symbolic_module :
       name:string
    -> Symbolic_extern.Module.t
    -> Symbolic_extern.Func.t State.t
    -> Symbolic_extern.Func.t State.t

  val abstract_module :
       name:string
    -> Abstract_extern.Module.t
    -> Abstract_extern.Func.t State.t
    -> Abstract_extern.Func.t State.t
end

module Binary : sig
  (* TODO: change this to name:.. -> (state*module) -> (state*module) so that it can be piped easily *)
  (** link a module with a given link state, producing a runnable module and a
      new link state *)
  val modul :
       name:string option
    -> 'f State.t
    -> Binary.Module.t
    -> ('f Linked.Module.t * 'f State.t) Result.t
end

(** give a named to the last linked module in the given link state *)
val register_last_module :
  'f State.t -> name:string -> id:string option -> 'f State.t Result.t
