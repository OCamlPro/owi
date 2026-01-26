include Smtml.Typed.Float64

let of_bits x = Smtml.Typed.Float64.reinterpret_i64 x

let to_bits x = Smtml.Typed.Float64.to_bv x
