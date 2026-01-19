module type T = sig
  type t

  type value

  type 'a choice

  val value : t -> value

  val set_value : t -> value -> unit choice
end
