(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

(** Module providing functions to parse a wasm script from various kind of
    inputs. *)

val guess_from_file :
     Fpath.t
  -> ((Text.modul, Text.script) Either.t, Simplified.modul) Either.t Result.t

module Text : sig
  module Script : sig
    (** Parse a script from a string. *)
    val from_string : string -> Text.script Result.t

    (** Parse a script from a channel. *)
    val from_channel : in_channel -> Text.script Result.t

    (** Parse a script from a file. *)
    val from_file : Fpath.t -> Text.script Result.t
  end

  module Module : sig
    (** Parse a module from a string. *)
    val from_string : string -> Text.modul Result.t

    (** Parse a module from a channel. *)
    val from_channel : in_channel -> Text.modul Result.t

    (** Parse a module from a file. *)
    val from_file : Fpath.t -> Text.modul Result.t
  end

  module Inline_module : sig
    (** Parse an inline module from a string. *)
    val from_string : string -> Text.modul Result.t

    (** Parse an inline module from a channel. *)
    val from_channel : in_channel -> Text.modul Result.t

    (** Parse an inline module from a file. *)
    val from_file : Fpath.t -> Text.modul Result.t
  end
end

module Binary : sig
  module Module : sig
    (** Parse a module from a string. *)
    val from_string : string -> Simplified.modul Result.t

    (** Parse a module from a channel. *)
    val from_channel : in_channel -> Simplified.modul Result.t

    (** Parse a module from a file. *)
    val from_file : Fpath.t -> Simplified.modul Result.t
  end
end
