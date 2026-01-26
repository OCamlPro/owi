include Smtml.Typed.Float64

let of_concrete (f : Float64.t) : t = Smtml.Typed.Float64.v (Float64.to_bits f)

let zero = of_concrete Float64.zero

let of_bits x = Smtml.Typed.Float64.reinterpret_i64 x

let to_bits x = Smtml.Typed.Float64.to_bv x
