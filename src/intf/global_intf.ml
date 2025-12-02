module type T = sig
  module Value : Value_intf.T

  type t

  val value : t -> Value.t

  val set_value : t -> Value.t -> unit
end
