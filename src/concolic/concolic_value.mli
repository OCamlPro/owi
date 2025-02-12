type externref

include
  Value_intf.T
    with type bool = Concrete.Value.bool * Symbolic_value.bool
     and type int32 = Concrete.Value.int32 * Symbolic_value.int32
     and type int64 = Concrete.Value.int64 * Symbolic_value.int64
     and type float32 = Concrete.Value.float32 * Symbolic_value.float32
     and type float64 = Concrete.Value.float64 * Symbolic_value.float64
     and type ref_value = Concrete.Value.ref_value * Symbolic_value.ref_value

module Bool : sig
  include module type of Bool
end

val pair_value : Concrete.Value.t -> Symbolic_value.t -> t

val concrete_value : t -> Concrete_value.t

val symbolic_value : t -> Symbolic_value.t
