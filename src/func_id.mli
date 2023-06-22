
type t
type 'a collection

val empty : 'a collection
val add : 'a -> 'a collection -> t * 'a collection

val get : t -> 'a collection -> 'a

val pp : Format.formatter -> t -> unit
