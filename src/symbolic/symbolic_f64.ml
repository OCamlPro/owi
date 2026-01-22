include Smtml.Typed.Float64

type t = Smtml.Typed.float64 Smtml.Typed.t

let of_concrete (f : Float64.t) : t = Smtml.Typed.Float64.v (Float64.to_bits f)

let zero = of_concrete Float64.zero

let copy_sign x y = Smtml.Typed.Float64.copysign x y

let of_bits x = Smtml.Typed.Float64.reinterpret_i64 x

let to_bits x = Smtml.Typed.Float64.to_bv x

let of_float (f : Float.t) : t = Concrete_f64.of_float f |> of_concrete
