(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

(** Utility functions to compile a module until a given step. *)

module Any : sig
  val until_binary_validate :
    unsafe:bool -> 'extern_func Kind.t -> Binary.modul Result.t

  val until_optimize :
    unsafe:bool -> optimize:bool -> 'extern_func Kind.t -> Binary.modul Result.t

  (** compile a module with a given link state and produce a new link state and
      a runnable module *)
  val until_link :
       unsafe:bool
    -> optimize:bool
    -> name:string option
    -> 'extern_func Link.state
    -> 'extern_func Kind.t
    -> ('extern_func Link.module_to_run * 'extern_func Link.state) Result.t

  (** compile and interpret a module with a given link state and produce a new
      link state *)
  val until_interpret :
       unsafe:bool
    -> optimize:bool
    -> name:string option
    -> Concrete_value.Func.extern_func Link.state
    -> Concrete_value.Func.extern_func Kind.t
    -> Concrete_value.Func.extern_func Link.state Result.t
end

module File : sig
  val until_binary_validate : unsafe:bool -> Fpath.t -> Binary.modul Result.t

  val until_optimize :
    unsafe:bool -> optimize:bool -> Fpath.t -> Binary.modul Result.t

  (** compile a file with a given link state and produce a new link state and a
      runnable module *)
  val until_link :
       unsafe:bool
    -> optimize:bool
    -> name:string option
    -> 'extern_func Link.state
    -> Fpath.t
    -> ('extern_func Link.module_to_run * 'extern_func Link.state) Result.t

  (** compile and interpret a file with a given link state and produce a new
      link state *)
  val until_interpret :
       unsafe:bool
    -> optimize:bool
    -> name:string option
    -> Concrete_value.Func.extern_func Link.state
    -> Fpath.t
    -> Concrete_value.Func.extern_func Link.state Result.t
end

module Text : sig
  val until_text_validate : unsafe:bool -> Text.modul -> Text.modul Result.t

  val until_binary : unsafe:bool -> Text.modul -> Binary.modul Result.t

  val until_binary_validate : unsafe:bool -> Text.modul -> Binary.modul Result.t

  val until_optimize :
    unsafe:bool -> optimize:bool -> Text.modul -> Binary.modul Result.t

  (** compile a module with a given link state and produce a new link state and
      a runnable module *)
  val until_link :
       unsafe:bool
    -> optimize:bool
    -> name:string option
    -> 'f Link.state
    -> Text.modul
    -> ('f Link.module_to_run * 'f Link.state) Result.t

  (** compile and interpret a module with a given link state and produce a new
      link state *)
  val until_interpret :
       unsafe:bool
    -> optimize:bool
    -> name:string option
    -> Concrete_value.Func.extern_func Link.state
    -> Text.modul
    -> Concrete_value.Func.extern_func Link.state Result.t
end

module Binary : sig
  val until_binary_validate :
    unsafe:bool -> Binary.modul -> Binary.modul Result.t

  val until_optimize :
    unsafe:bool -> optimize:bool -> Binary.modul -> Binary.modul Result.t

  (** compile a module with a given link state and produce a new link state and
      a runnable module *)
  val until_link :
       unsafe:bool
    -> optimize:bool
    -> name:string option
    -> 'f Link.state
    -> Binary.modul
    -> ('f Link.module_to_run * 'f Link.state) Result.t

  (** compile and interpret a module with a given link state and produce a new
      link state *)
  val until_interpret :
       unsafe:bool
    -> optimize:bool
    -> name:string option
    -> Concrete_value.Func.extern_func Link.state
    -> Binary.modul
    -> Concrete_value.Func.extern_func Link.state Result.t
end
