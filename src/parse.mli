(** Module providing functions to parse a wasm script from various kind of
    inputs. *)

(** Parse a script from a string. *)
val from_string : string -> (Types.script, string) result

(** Parse a script from a channel. *)
val from_channel : in_channel -> (Types.script, string) result

(** Parse a script from a file. *)
val from_file : filename:string -> (Types.script, string) result
