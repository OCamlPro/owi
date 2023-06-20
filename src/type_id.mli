(** Dynamic type equalities *)

type ('a, 'b) eq = Eq : ('a, 'a) eq

type 'a ty

val fresh : string -> 'a ty

val name : 'a ty -> string

val eq : 'a ty -> 'b ty -> ('a, 'b) eq option
