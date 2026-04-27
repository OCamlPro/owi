module Make (M : sig
  type t
end) : sig
  type t = M.t list

  val empty : M.t list

  val pop : t -> M.t * t

  val push : t -> M.t -> t

  val pop_n : t -> int -> M.t list * t

  val pop_2 : t -> M.t * M.t * t

  val pp : M.t Fmt.t -> Format.formatter -> t -> unit

  val is_empty : t -> bool

  val to_list : t -> M.t list

  val of_list : M.t list -> t

  val append : t -> t -> t

  val take : int -> t -> t

  val drop : int -> t -> t

  val rev : t -> t

  val length : t -> int
end
