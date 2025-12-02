module type T = sig
  module Value : Value_intf.T

  type t

  val get : t -> int -> Value.Ref.t

  val set : t -> int -> Value.Ref.t -> unit

  val size : t -> int

  val typ : t -> Text.ref_type

  val max_size : t -> int option

  val grow : t -> Int32.t -> Value.Ref.t -> unit

  val fill : t -> Int32.t -> Int32.t -> Value.Ref.t -> unit

  val copy :
    t_src:t -> t_dst:t -> src:Int32.t -> dst:Int32.t -> len:Int32.t -> unit
end
