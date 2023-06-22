
type t
type 'a collection

val empty : 'a collection
val add : 'a -> Simplified.func_type -> 'a collection -> t * 'a collection

val get : t -> 'a collection -> 'a

val get_typ : t -> 'a collection -> Simplified.func_type

val pp : Format.formatter -> t -> unit
