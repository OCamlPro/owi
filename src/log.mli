val debug_on : bool ref

val debug : ('a, Format.formatter, unit) format -> 'a

val err : ('a, Format.formatter, unit, 'b) format4 -> 'a
