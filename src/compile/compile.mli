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
    -> Env.Concrete.t
    -> Concrete_extern.Func.t Kind.t
    -> (Env.Concrete.modul * Env.Concrete.t) Result.t

  val until_symbolic_link :
       unsafe:bool
    -> name:string option
    -> Env.Symbolic.t
    -> Symbolic_extern.Func.t Kind.t
    -> (Env.Symbolic.modul * Env.Symbolic.t) Result.t

  val until_abstract_link :
       unsafe:bool
    -> name:string option
    -> Env.Abstract.t
    -> Abstract_extern.Func.t Kind.t
    -> (Env.Abstract.modul * Env.Abstract.t) Result.t
end

module File : sig
  val until_binary : unsafe:bool -> Fpath.t -> Binary.Module.t Result.t

  val until_validate : unsafe:bool -> Fpath.t -> Binary.Module.t Result.t

  (** compile a file with a given link state and produce a new link state and a
      runnable module *)
  val until_concrete_link :
       unsafe:bool
    -> name:string option
    -> Env.Concrete.t
    -> Fpath.t
    -> (Env.Concrete.modul * Env.Concrete.t) Result.t

  (** compile a file with a given link state and produce a new link state and a
      runnable module *)
  val until_symbolic_link :
       unsafe:bool
    -> name:string option
    -> Env.Symbolic.t
    -> Fpath.t
    -> (Env.Symbolic.modul * Env.Symbolic.t) Result.t

  (** compile a file with a given link state and produce a new link state and a
      runnable module *)
  val until_abstract_link :
       unsafe:bool
    -> name:string option
    -> Env.Abstract.t
    -> Fpath.t
    -> (Env.Abstract.modul * Env.Abstract.t) Result.t
end

module Text : sig
  val until_binary : unsafe:bool -> Text.Module.t -> Binary.Module.t Result.t

  val until_validate : unsafe:bool -> Text.Module.t -> Binary.Module.t Result.t

  (** compile a module with a given link state and produce a new link state and
      a runnable module *)
  val until_concrete_link :
       unsafe:bool
    -> name:string option
    -> Env.Concrete.t
    -> Text.Module.t
    -> (Env.Concrete.modul * Env.Concrete.t) Result.t

  (** compile a module with a given link state and produce a new link state and
      a runnable module *)
  val until_symbolic_link :
       unsafe:bool
    -> name:string option
    -> Env.Symbolic.t
    -> Text.Module.t
    -> (Env.Symbolic.modul * Env.Symbolic.t) Result.t

  (** compile a module with a given link state and produce a new link state and
      a runnable module *)
  val until_abstract_link :
       unsafe:bool
    -> name:string option
    -> Env.Abstract.t
    -> Text.Module.t
    -> (Env.Abstract.modul * Env.Abstract.t) Result.t
end

module Binary : sig
  val until_validate :
    unsafe:bool -> Binary.Module.t -> Binary.Module.t Result.t

  (** compile a module with a given link state and produce a new link state and
      a runnable module *)
  val until_concrete_link :
       unsafe:bool
    -> name:string option
    -> Env.Concrete.t
    -> Binary.Module.t
    -> (Env.Concrete.modul * Env.Concrete.t) Result.t

  (** compile a module with a given link state and produce a new link state and
      a runnable module *)
  val until_symbolic_link :
       unsafe:bool
    -> name:string option
    -> Env.Symbolic.t
    -> Binary.Module.t
    -> (Env.Symbolic.modul * Env.Symbolic.t) Result.t

  (** compile a module with a given link state and produce a new link state and
      a runnable module *)
  val until_abstract_link :
       unsafe:bool
    -> name:string option
    -> Env.Abstract.t
    -> Binary.Module.t
    -> (Env.Abstract.modul * Env.Abstract.t) Result.t
end
