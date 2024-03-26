(** Utility functions to work with utf8. *)

val check_utf8 : string -> unit Result.t

val encode : int list -> string
