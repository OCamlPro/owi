(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

(** Module to link a binary/extern module and producing a runnable module along
    with a link state. *)

(** Runtime module *)

module Linked_module : sig
  type 'ext t

  val get_memory : _ t -> int -> Concrete_memory.t Concrete_choice.t

  val get_func : _ t -> int -> Kind.func

  val get_table : _ t -> int -> Concrete_table.t Concrete_choice.t

  val get_elem : _ t -> int -> Concrete_elem.t

  val get_data : _ t -> int -> Concrete_data.t Concrete_choice.t

  val get_global : _ t -> int -> Concrete_global.t Concrete_choice.t

  val get_extern_func : 'ext t -> int -> 'ext

  val get_id : _ t -> int

  val fold_globals : (int -> Concrete_global.t -> 'a -> 'a) -> 'a -> 'b t -> 'a

  val get_expr_to_run : _ t -> Binary.expr Annotated.t list
end

(* Link State *)

module StringMap : Map.S with type key = string

module StringSet : Set.S

module State : sig
  type 'extern modules = 'extern Linked_module.t Dynarray.t

  type 'extern t

  (** the empty link state *)
  val empty : unit -> 'extern t

  val get_modules : 'extern t -> 'extern modules

  val get_global_from_module :
    _ t -> string option -> string -> Concrete_global.t Result.t

  val get_func_from_module :
    _ t -> string option -> string -> (Kind.func * int) Result.t

  (** give a named to the last linked module in the given link state *)
  val register_last_module :
    'extern t -> name:string -> id:string option -> 'extern t Result.t
end

(* Link *)

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
  val concrete_module :
       name:string option
    -> Concrete_extern.Func.t State.t
    -> Binary.Module.t
    -> (Concrete_extern.Func.t Linked_module.t * Concrete_extern.Func.t State.t)
       Result.t

  val symbolic_module :
       name:string option
    -> Symbolic_extern.Func.t State.t
    -> Binary.Module.t
    -> (Symbolic_extern.Func.t Linked_module.t * Symbolic_extern.Func.t State.t)
       Result.t

  val abstract_module :
       name:string option
    -> Abstract_extern.Func.t State.t
    -> Binary.Module.t
    -> (Abstract_extern.Func.t Linked_module.t * Abstract_extern.Func.t State.t)
       Result.t
end
