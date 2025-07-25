type t

val random : t

val default : t

val compute : instr_counter:int -> t

val compare : t -> t -> int
