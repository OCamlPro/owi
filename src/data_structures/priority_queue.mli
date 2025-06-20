type 'a t

val empty : 'a t

val is_empty : 'a t -> bool

val pop : 'a t -> 'a option * 'a t

val push : 'a -> 'a t -> 'a t
