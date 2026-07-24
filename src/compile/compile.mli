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
    -> Concrete_env.t
    -> Concrete_extern.Func.t Kind.t
    -> (int * Concrete_env.t) Result.t

  val until_symbolic_link :
       unsafe:bool
    -> name:string option
    -> Symbolic_env.t
    -> Symbolic_extern.Func.t Kind.t
    -> (int * Symbolic_env.t) Result.t

  val until_abstract_link :
       unsafe:bool
    -> name:string option
    -> Abstract_env.t
    -> Abstract_extern.Func.t Kind.t
    -> (int * Abstract_env.t) Result.t
end

module File : sig
  val until_binary : unsafe:bool -> Fpath.t -> Binary.Module.t Result.t

  val until_validate : unsafe:bool -> Fpath.t -> Binary.Module.t Result.t

  (** compile a file with a given link state and produce a new link state and a
      runnable module *)
  val until_concrete_link :
       unsafe:bool
    -> name:string option
    -> Concrete_env.t
    -> Fpath.t
    -> (int * Concrete_env.t) Result.t

  (** compile a file with a given link state and produce a new link state and a
      runnable module *)
  val until_symbolic_link :
       unsafe:bool
    -> name:string option
    -> Symbolic_env.t
    -> Fpath.t
    -> (int * Symbolic_env.t) Result.t

  (** compile a file with a given link state and produce a new link state and a
      runnable module *)
  val until_abstract_link :
       unsafe:bool
    -> name:string option
    -> Abstract_env.t
    -> Fpath.t
    -> (int * Abstract_env.t) Result.t
end

module Text : sig
  val until_binary : unsafe:bool -> Text.Module.t -> Binary.Module.t Result.t

  val until_validate : unsafe:bool -> Text.Module.t -> Binary.Module.t Result.t

  (** compile a module with a given link state and produce a new link state and
      a runnable module *)
  val until_concrete_link :
       unsafe:bool
    -> name:string option
    -> Concrete_env.t
    -> Text.Module.t
    -> (int * Concrete_env.t) Result.t

  (** compile a module with a given link state and produce a new link state and
      a runnable module *)
  val until_symbolic_link :
       unsafe:bool
    -> name:string option
    -> Symbolic_env.t
    -> Text.Module.t
    -> (int * Symbolic_env.t) Result.t

  (** compile a module with a given link state and produce a new link state and
      a runnable module *)
  val until_abstract_link :
       unsafe:bool
    -> name:string option
    -> Abstract_env.t
    -> Text.Module.t
    -> (int * Abstract_env.t) Result.t
end

module Binary : sig
  val until_validate :
    unsafe:bool -> Binary.Module.t -> Binary.Module.t Result.t

  (** compile a module with a given link state and produce a new link state and
      a runnable module *)
  val until_concrete_link :
       unsafe:bool
    -> name:string option
    -> Concrete_env.t
    -> Binary.Module.t
    -> (int * Concrete_env.t) Result.t

  (** compile a module with a given link state and produce a new link state and
      a runnable module *)
  val until_symbolic_link :
       unsafe:bool
    -> name:string option
    -> Symbolic_env.t
    -> Binary.Module.t
    -> (int * Symbolic_env.t) Result.t

  (** compile a module with a given link state and produce a new link state and
      a runnable module *)
  val until_abstract_link :
       unsafe:bool
    -> name:string option
    -> Abstract_env.t
    -> Binary.Module.t
    -> (int * Abstract_env.t) Result.t
end
