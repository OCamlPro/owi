
type t
type 'a collection

val empty : 'a collection
val with_fresh_id : (t -> ('a * 'b) Result.t) -> 'a collection -> ('a collection * 'b) Result.t

val get : t -> 'a collection -> 'a

val pp : Format.formatter -> t -> unit
