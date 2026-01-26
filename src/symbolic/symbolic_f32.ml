include Smtml.Typed.Float32

let of_float32 (f : Float32.t) : t = Smtml.Typed.Float32.v (Float32.to_bits f)

let of_bits x = Smtml.Typed.Float32.reinterpret_i32 x

let to_bits x = Smtml.Typed.Float32.to_bv x
