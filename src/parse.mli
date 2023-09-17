(** Module providing functions to parse a wasm script from various kind of
    inputs. *)

module Script : sig
  (** Parse a script from a string. *)
  val from_string : string -> Text.script Result.t

  (** Parse a script from a channel. *)
  val from_channel : in_channel -> Text.script Result.t

  (** Parse a script from a file. *)
  val from_file : filename:string -> Text.script Result.t
end

module Module : sig
  (** Parse a module from a string. *)
  val from_string : string -> Text.modul Result.t

  (** Parse a module from a channel. *)
  val from_channel : in_channel -> Text.modul Result.t

  (** Parse a module from a file. *)
  val from_file : filename:string -> Text.modul Result.t
end

module Inline_module : sig
  (** Parse an inline module from a string. *)
  val from_string : string -> Text.modul Result.t

  (** Parse an inline module from a channel. *)
  val from_channel : in_channel -> Text.modul Result.t

  (** Parse an inline module from a file. *)
  val from_file : filename:string -> Text.modul Result.t
end
