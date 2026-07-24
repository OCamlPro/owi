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
    -> Link.Concrete.t
    -> Concrete_extern.Func.t Kind.t
    -> (int * Link.Concrete.t) Result.t

  val until_symbolic_link :
       unsafe:bool
    -> name:string option
    -> Link.Symbolic.t
    -> Symbolic_extern.Func.t Kind.t
    -> (int * Link.Symbolic.t) Result.t

  val until_abstract_link :
       unsafe:bool
    -> name:string option
    -> Link.Abstract.t
    -> Abstract_extern.Func.t Kind.t
    -> (int * Link.Abstract.t) Result.t
end

module File : sig
  val until_binary : unsafe:bool -> Fpath.t -> Binary.Module.t Result.t

  val until_validate : unsafe:bool -> Fpath.t -> Binary.Module.t Result.t

  (** compile a file with a given link state and produce a new link state and a
      runnable module *)
  val until_concrete_link :
       unsafe:bool
    -> name:string option
    -> Link.Concrete.t
    -> Fpath.t
    -> (int * Link.Concrete.t) Result.t

  (** compile a file with a given link state and produce a new link state and a
      runnable module *)
  val until_symbolic_link :
       unsafe:bool
    -> name:string option
    -> Link.Symbolic.t
    -> Fpath.t
    -> (int * Link.Symbolic.t) Result.t

  (** compile a file with a given link state and produce a new link state and a
      runnable module *)
  val until_abstract_link :
       unsafe:bool
    -> name:string option
    -> Link.Abstract.t
    -> Fpath.t
    -> (int * Link.Abstract.t) Result.t
end

module Text : sig
  val until_binary : unsafe:bool -> Text.Module.t -> Binary.Module.t Result.t

  val until_validate : unsafe:bool -> Text.Module.t -> Binary.Module.t Result.t

  (** compile a module with a given link state and produce a new link state and
      a runnable module *)
  val until_concrete_link :
       unsafe:bool
    -> name:string option
    -> Link.Concrete.t
    -> Text.Module.t
    -> (int * Link.Concrete.t) Result.t

  (** compile a module with a given link state and produce a new link state and
      a runnable module *)
  val until_symbolic_link :
       unsafe:bool
    -> name:string option
    -> Link.Symbolic.t
    -> Text.Module.t
    -> (int * Link.Symbolic.t) Result.t

  (** compile a module with a given link state and produce a new link state and
      a runnable module *)
  val until_abstract_link :
       unsafe:bool
    -> name:string option
    -> Link.Abstract.t
    -> Text.Module.t
    -> (int * Link.Abstract.t) Result.t
end

module Binary : sig
  val until_validate :
    unsafe:bool -> Binary.Module.t -> Binary.Module.t Result.t

  (** compile a module with a given link state and produce a new link state and
      a runnable module *)
  val until_concrete_link :
       unsafe:bool
    -> name:string option
    -> Link.Concrete.t
    -> Binary.Module.t
    -> (int * Link.Concrete.t) Result.t

  (** compile a module with a given link state and produce a new link state and
      a runnable module *)
  val until_symbolic_link :
       unsafe:bool
    -> name:string option
    -> Link.Symbolic.t
    -> Binary.Module.t
    -> (int * Link.Symbolic.t) Result.t

  (** compile a module with a given link state and produce a new link state and
      a runnable module *)
  val until_abstract_link :
       unsafe:bool
    -> name:string option
    -> Link.Abstract.t
    -> Binary.Module.t
    -> (int * Link.Abstract.t) Result.t
end
