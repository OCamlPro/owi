module Make (Prio : sig
  type t

  val compare : t -> t -> int
end) : sig
  type 'a t

  val empty : unit -> 'a t

  val is_empty : 'a t -> bool

  val pop : 'a t -> 'a option

  val push : Prio.t * 'a -> 'a t -> unit
end
