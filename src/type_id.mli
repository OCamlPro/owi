(** Dynamic type equalities *)

type 'a ty

val fresh : string -> 'a ty

val name : 'a ty -> string

val eq : 'a ty -> 'b ty -> ('a, 'b) Type.eq option
