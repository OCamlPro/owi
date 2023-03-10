(** Module to enable or disable the printing of debug logs. *)

(** wether debug printing is enabled or not *)
val debug_on : bool ref

(** print some debug info *)
val debug0 : (unit, Format.formatter, unit) format -> unit

val debug1 : ('a -> unit, Format.formatter, unit) format -> 'a -> unit

val debug2 : ('a -> 'b -> unit, Format.formatter, unit) format -> 'a -> 'b -> unit

(** print some debug info *)
val debug : ('a, Format.formatter, unit) format -> 'a

(** print some error and exit *)
val err : ('a, Format.formatter, unit, 'b) format4 -> 'a
