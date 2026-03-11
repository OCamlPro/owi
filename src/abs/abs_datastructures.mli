module Stack : sig
  type 'el t

  val empty : 'el t

  val pop : 'el t -> 'el * 'el t

  val push : 'el t -> 'el -> 'el t

  val pop_n : 'el t -> int -> 'el t * 'el t

  val pp : 'el Fmt.t -> Format.formatter -> 'el t -> unit

  val is_empty : 'el t -> bool

  val of_list : 'el list -> 'el t

  val to_list : 'el t -> 'el list
  val append : 'el t -> 'el t -> 'el t
  val take : int -> 'el t -> 'el t
  val rev : 'el t -> 'el t
end
