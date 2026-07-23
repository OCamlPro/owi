(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

(** Utility functions to compile a module until a given step. *)

module Any : sig
  val until_validate : unsafe:bool -> _ Kind.t -> Binary.Module.t Result.t

  (** compile a module with a given link state and produce a new link state and
      a runnable module *)
  val until_concrete_link :
       unsafe:bool
    -> name:string option
    -> Concrete_extern.Func.t Link.State.t
    -> Concrete_extern.Func.t Kind.t
    -> ( Concrete_extern.Func.t Link.Linked_module.t
       * Concrete_extern.Func.t Link.State.t )
       Result.t

  val until_symbolic_link :
       unsafe:bool
    -> name:string option
    -> Symbolic_extern.Func.t Link.State.t
    -> Symbolic_extern.Func.t Kind.t
    -> ( Symbolic_extern.Func.t Link.Linked_module.t
       * Symbolic_extern.Func.t Link.State.t )
       Result.t

  val until_abstract_link :
       unsafe:bool
    -> name:string option
    -> Abstract_extern.Func.t Link.State.t
    -> Abstract_extern.Func.t Kind.t
    -> ( Abstract_extern.Func.t Link.Linked_module.t
       * Abstract_extern.Func.t Link.State.t )
       Result.t
end

module File : sig
  val until_binary : unsafe:bool -> Fpath.t -> Binary.Module.t Result.t

  val until_validate : unsafe:bool -> Fpath.t -> Binary.Module.t Result.t

  (** compile a file with a given link state and produce a new link state and a
      runnable module *)
  val until_concrete_link :
       unsafe:bool
    -> name:string option
    -> Concrete_extern.Func.t Link.State.t
    -> Fpath.t
    -> ( Concrete_extern.Func.t Link.Linked_module.t
       * Concrete_extern.Func.t Link.State.t )
       Result.t

  (** compile a file with a given link state and produce a new link state and a
      runnable module *)
  val until_symbolic_link :
       unsafe:bool
    -> name:string option
    -> Symbolic_extern.Func.t Link.State.t
    -> Fpath.t
    -> ( Symbolic_extern.Func.t Link.Linked_module.t
       * Symbolic_extern.Func.t Link.State.t )
       Result.t

  (** compile a file with a given link state and produce a new link state and a
      runnable module *)
  val until_abstract_link :
       unsafe:bool
    -> name:string option
    -> Abstract_extern.Func.t Link.State.t
    -> Fpath.t
    -> ( Abstract_extern.Func.t Link.Linked_module.t
       * Abstract_extern.Func.t Link.State.t )
       Result.t
end

module Text : sig
  val until_binary : unsafe:bool -> Text.Module.t -> Binary.Module.t Result.t

  val until_validate : unsafe:bool -> Text.Module.t -> Binary.Module.t Result.t

  (** compile a module with a given link state and produce a new link state and
      a runnable module *)
  val until_concrete_link :
       unsafe:bool
    -> name:string option
    -> Concrete_extern.Func.t Link.State.t
    -> Text.Module.t
    -> ( Concrete_extern.Func.t Link.Linked_module.t
       * Concrete_extern.Func.t Link.State.t )
       Result.t

  (** compile a module with a given link state and produce a new link state and
      a runnable module *)
  val until_symbolic_link :
       unsafe:bool
    -> name:string option
    -> Symbolic_extern.Func.t Link.State.t
    -> Text.Module.t
    -> ( Symbolic_extern.Func.t Link.Linked_module.t
       * Symbolic_extern.Func.t Link.State.t )
       Result.t

  (** compile a module with a given link state and produce a new link state and
      a runnable module *)
  val until_abstract_link :
       unsafe:bool
    -> name:string option
    -> Abstract_extern.Func.t Link.State.t
    -> Text.Module.t
    -> ( Abstract_extern.Func.t Link.Linked_module.t
       * Abstract_extern.Func.t Link.State.t )
       Result.t
end

module Binary : sig
  val until_validate :
    unsafe:bool -> Binary.Module.t -> Binary.Module.t Result.t

  (** compile a module with a given link state and produce a new link state and
      a runnable module *)
  val until_concrete_link :
       unsafe:bool
    -> name:string option
    -> Concrete_extern.Func.t Link.State.t
    -> Binary.Module.t
    -> ( Concrete_extern.Func.t Link.Linked_module.t
       * Concrete_extern.Func.t Link.State.t )
       Result.t

  (** compile a module with a given link state and produce a new link state and
      a runnable module *)
  val until_symbolic_link :
       unsafe:bool
    -> name:string option
    -> Symbolic_extern.Func.t Link.State.t
    -> Binary.Module.t
    -> ( Symbolic_extern.Func.t Link.Linked_module.t
       * Symbolic_extern.Func.t Link.State.t )
       Result.t

  (** compile a module with a given link state and produce a new link state and
      a runnable module *)
  val until_abstract_link :
       unsafe:bool
    -> name:string option
    -> Abstract_extern.Func.t Link.State.t
    -> Binary.Module.t
    -> ( Abstract_extern.Func.t Link.Linked_module.t
       * Abstract_extern.Func.t Link.State.t )
       Result.t
end
