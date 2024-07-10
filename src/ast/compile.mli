(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

(** Utility functions to compile a module until a given step. *)

module Text : sig
  val until_check : unsafe:bool -> Text.modul -> Text.modul Result.t

  val until_binary : unsafe:bool -> Text.modul -> Binary.modul Result.t

  val until_typecheck : unsafe:bool -> Text.modul -> Binary.modul Result.t

  val until_optimize :
    unsafe:bool -> optimize:bool -> Text.modul -> Binary.modul Result.t

  (** compile a module with a given link state and produce a new link state and
      a runnable module *)
  val until_link :
       unsafe:bool
    -> 'f Link.state
    -> optimize:bool
    -> name:string option
    -> Text.modul
    -> ('f Link.module_to_run * 'f Link.state) Result.t

  (** compile and interpret a module with a given link state and produce a new
      link state *)
  val until_interpret :
       Concrete_value.Func.extern_func Link.state
    -> unsafe:bool
    -> optimize:bool
    -> name:string option
    -> Text.modul
    -> Concrete_value.Func.extern_func Link.state Result.t
end

module Binary : sig
  val until_typecheck : unsafe:bool -> Binary.modul -> Binary.modul Result.t

  val until_optimize :
    unsafe:bool -> optimize:bool -> Binary.modul -> Binary.modul Result.t

  (** compile a module with a given link state and produce a new link state and
      a runnable module *)
  val until_link :
       unsafe:bool
    -> 'f Link.state
    -> optimize:bool
    -> name:string option
    -> Binary.modul
    -> ('f Link.module_to_run * 'f Link.state) Result.t

  (** compile and interpret a module with a given link state and produce a new
      link state *)
  val until_interpret :
       Concrete_value.Func.extern_func Link.state
    -> unsafe:bool
    -> optimize:bool
    -> name:string option
    -> Binary.modul
    -> Concrete_value.Func.extern_func Link.state Result.t
end

module ApiV2 : sig
  type 'a t

  val extern_module :
       func_typ:('a -> Types.binary Types.func_type)
    -> name:string
    -> module_impl:'a Link.extern_module
    -> 'a t

  val file :
       filename:Fpath.t
    -> unsafe:bool
    -> optimize:bool
    -> add_main_as_start:bool
    -> 'a t

  val files :
       filenames:Fpath.t list
    -> unsafe:bool
    -> optimize:bool
    -> add_main_as_start:bool
    -> 'a t

  val compile :
    'a t list -> ('a Link.module_to_run list * 'a Link.state) Result.t
end
