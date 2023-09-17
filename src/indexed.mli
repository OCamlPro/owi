type 'a t

val get : 'a t -> 'a
val get_index : 'a t -> int
val bind : 'a t -> ('a -> 'b t) -> 'b t
val map : ('a -> 'b) -> 'a t -> 'b t
val return : int -> 'a -> 'a t


val get_at : int -> 'a t list -> 'a option
val get_at_exn : int -> 'a t list -> 'a
val has_index : int -> 'a t -> bool

val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
