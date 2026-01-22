include Smtml.Typed.Float32

type t = Smtml.Typed.float32 Smtml.Typed.t

let of_concrete (f : Float32.t) : t = Smtml.Typed.Float32.v (Float32.to_bits f)

let zero = of_concrete Float32.zero

let of_bits x = Smtml.Typed.Float32.reinterpret_i32 x

let to_bits x = Smtml.Typed.Float32.to_bv x

let pp fmt x = Smtml.Typed.Float32.pp fmt x
