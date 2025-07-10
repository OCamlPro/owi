type t

val random : t

val default : t

val compute : instr_counter:int -> t

val to_int : t -> int
