type t = Smtml.Typed.float64 Smtml.Typed.t

let of_concrete (f : Float64.t) : t = Smtml.Typed.Float64.v (Float64.to_bits f)

let zero = of_concrete Float64.zero

let abs x = Smtml.Typed.Float64.abs x

let neg x = Smtml.Typed.Float64.neg x

let sqrt x = Smtml.Typed.Float64.sqrt x

let ceil x = Smtml.Typed.Float64.ceil x

let floor x = Smtml.Typed.Float64.floor x

let trunc x = Smtml.Typed.Float64.trunc x

let nearest x = Smtml.Typed.Float64.nearest x

let add x y = Smtml.Typed.Float64.add x y

let sub x y = Smtml.Typed.Float64.sub x y

let mul x y = Smtml.Typed.Float64.mul x y

let div x y = Smtml.Typed.Float64.div x y

let min x y = Smtml.Typed.Float64.min x y

let max x y = Smtml.Typed.Float64.max x y

let copy_sign x y = Smtml.Typed.Float64.copysign x y

let eq x y = Smtml.Typed.Float64.eq x y

let ne x y = Smtml.Typed.Float64.ne x y

let lt x y = Smtml.Typed.Float64.lt x y

let gt x y = Smtml.Typed.Float64.gt x y

let le x y = Smtml.Typed.Float64.le x y

let ge x y = Smtml.Typed.Float64.ge x y

let convert_i32_s x = Smtml.Typed.Float64.convert_i32_s x

let convert_i32_u x = Smtml.Typed.Float64.convert_i32_u x

let convert_i64_s x = Smtml.Typed.Float64.convert_i64_s x

let convert_i64_u x = Smtml.Typed.Float64.convert_i64_u x

let promote_f32 x = Smtml.Typed.Float64.promote_f32 x

let reinterpret_i64 x = Smtml.Typed.Float64.reinterpret_i64 x

let of_bits x = Smtml.Typed.Float64.reinterpret_i64 x

let to_bits x = Smtml.Typed.Float64.to_bv x

let pp fmt x = Smtml.Typed.Float64.pp fmt (Obj.magic x)

let of_float (f : Float.t) : t = Concrete_f64.of_float f |> of_concrete
