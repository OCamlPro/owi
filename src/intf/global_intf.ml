module type T = sig
  type t

  type value

  val value : t -> value

  val set_value : t -> value -> unit
end
