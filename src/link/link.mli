(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

(** Module to link a binary/extern module and producing a runnable module along
    with a link state. *)

(** Runtime module *)

module Linked_module : sig
  type 'ext t

  val get_func : _ t -> int -> Kind.func

  val get_table : _ t -> int -> Concrete_table.t Concrete_choice.t

  val get_elem : _ t -> int -> Concrete_elem.t

  val get_global : _ t -> int -> Concrete_global.t Concrete_choice.t

  val get_extern_func : 'ext t -> int -> 'ext

  val get_id : _ t -> int

  val fold_globals : (int -> Concrete_global.t -> 'a -> 'a) -> 'a -> 'b t -> 'a

  val get_init_code : _ t -> Binary.expr Annotated.t
end

(* Link State *)

module State : sig
  type 'extern t

  (** the empty link state *)
  val empty : unit -> _ t

  val get_memory :
    _ t -> modul:int -> int -> Concrete_memory.t Concrete_choice.t

  val get_data : _ t -> modul:int -> int -> Concrete_data.t Concrete_choice.t

  (* TODO: remove this! *)
  val get_module : 'extern t -> int -> 'extern Linked_module.t

  val get_exported_global :
       _ t
    -> module_name:string option
    -> global_name:string
    -> Concrete_global.t Result.t

  val get_exported_func :
       _ t
    -> module_name:string option
    -> func_name:string
    -> (Kind.func * int) Result.t

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
    -> (int * Concrete_extern.Func.t State.t) Result.t

  val symbolic_module :
       name:string option
    -> Symbolic_extern.Func.t State.t
    -> Binary.Module.t
    -> (int * Symbolic_extern.Func.t State.t) Result.t

  val abstract_module :
       name:string option
    -> Abstract_extern.Func.t State.t
    -> Binary.Module.t
    -> (int * Abstract_extern.Func.t State.t) Result.t
end
