(** Module providing functions to parse a wasm script from various kind of
    inputs. *)

(** Parse a module from a string. *)
val module_from_string : string -> (Types.Symbolic.modul, string) result

(** Parse a module from a channel. *)
val module_from_channel : in_channel -> (Types.Symbolic.modul, string) result

(** Parse a module from a file. *)
val module_from_file : filename:string -> (Types.Symbolic.modul, string) result

(** Parse a script from a string. *)
val script_from_string : string -> (Types.Symbolic.script, string) result

(** Parse a script from a channel. *)
val script_from_channel : in_channel -> (Types.Symbolic.script, string) result

(** Parse a script from a file. *)
val script_from_file : filename:string -> (Types.Symbolic.script, string) result
