(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

(** Utility functions to compile a module until a given step. *)

module Any : sig
  val until_validate : unsafe:bool -> 'f Kind.t -> Binary.Module.t Result.t

  (** compile a module with a given link state and produce a new link state and
      a runnable module *)
  val until_link :
       unsafe:bool
    -> name:string option
    -> 'f Link.State.t
    -> 'f Kind.t
    -> ('f Linked.Module.t * 'f Link.State.t) Result.t
end

module File : sig
  val until_binary : unsafe:bool -> Fpath.t -> Binary.Module.t Result.t

  val until_validate : unsafe:bool -> Fpath.t -> Binary.Module.t Result.t

  (** compile a file with a given link state and produce a new link state and a
      runnable module *)
  val until_link :
       unsafe:bool
    -> name:string option
    -> 'f Link.State.t
    -> Fpath.t
    -> ('f Linked.Module.t * 'f Link.State.t) Result.t
end

module Text : sig
  val until_binary : unsafe:bool -> Text.Module.t -> Binary.Module.t Result.t

  val until_validate : unsafe:bool -> Text.Module.t -> Binary.Module.t Result.t

  (** compile a module with a given link state and produce a new link state and
      a runnable module *)
  val until_link :
       unsafe:bool
    -> name:string option
    -> 'f Link.State.t
    -> Text.Module.t
    -> ('f Linked.Module.t * 'f Link.State.t) Result.t
end

module Binary : sig
  val until_validate :
    unsafe:bool -> Binary.Module.t -> Binary.Module.t Result.t

  (** compile a module with a given link state and produce a new link state and
      a runnable module *)
  val until_link :
       unsafe:bool
    -> name:string option
    -> 'f Link.State.t
    -> Binary.Module.t
    -> ('f Linked.Module.t * 'f Link.State.t) Result.t
end
