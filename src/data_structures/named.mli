type 'a t =
  { values : 'a Indexed.t list
  ; named : int String_map.t
  }

val fold : (int -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

val map : ('a Indexed.t -> 'b Indexed.t) -> 'a t -> 'b t
