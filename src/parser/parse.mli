(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

(** Module providing functions to parse a wasm script from various kind of
    inputs. *)

val guess_from_file : Fpath.t -> 'extern_func Kind.t Result.t

module Text : sig
  module Script : sig
    (** Parse a script from a string. *)
    val from_string : string -> Wast.script Result.t

    (** Parse a script from a channel. *)
    val from_channel : in_channel -> Wast.script Result.t

    (** Parse a script from a file. *)
    val from_file : Fpath.t -> Wast.script Result.t
  end

  module Module : sig
    (** Parse a module from a string. *)
    val from_string : string -> Text.Module.t Result.t

    (** Parse a module from a channel. *)
    val from_channel : in_channel -> Text.Module.t Result.t

    (** Parse a module from a file. *)
    val from_file : Fpath.t -> Text.Module.t Result.t
  end

  module Inline_module : sig
    (** Parse an inline module from a string. *)
    val from_string : string -> Text.Module.t Result.t

    (** Parse an inline module from a channel. *)
    val from_channel : in_channel -> Text.Module.t Result.t

    (** Parse an inline module from a file. *)
    val from_file : Fpath.t -> Text.Module.t Result.t
  end
end

module Binary : sig
  module Module : sig
    (** Parse a module from a string. *)
    val from_string : string -> Binary.Module.t Result.t

    (** Parse a module from a channel. *)
    val from_channel : in_channel -> Binary.Module.t Result.t

    (** Parse a module from a file. *)
    val from_file : Fpath.t -> Binary.Module.t Result.t
  end
end
