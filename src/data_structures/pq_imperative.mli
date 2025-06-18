type 'a t

val empty : unit -> 'a t

val is_empty : 'a t -> bool

val pop : 'a t -> 'a option

val push : 'a -> 'a t -> unit
