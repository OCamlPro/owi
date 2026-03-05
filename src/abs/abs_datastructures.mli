module Stack : sig
  type t = Abs_value.t list

  val pop : t -> Abs_value.t * t

  val push : t -> Abs_value.t -> t

  val pop_n : t -> int -> t * t
end
