type 'a t

val init : (int * 'a) list -> (int * int * string option) list -> 'a t

val pp_graph : Format.formatter -> 'a Types.instr Annotated.t list t -> unit
