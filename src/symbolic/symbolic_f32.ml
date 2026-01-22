type t = Smtml.Typed.float32 Smtml.Typed.t

let of_concrete (f : Float32.t) : t = Smtml.Typed.Float32.v (Float32.to_bits f)

let zero = of_concrete Float32.zero

let abs x = Smtml.Typed.Float32.abs x

let neg x = Smtml.Typed.Float32.neg x

let sqrt x = Smtml.Typed.Float32.sqrt x

let ceil x = Smtml.Typed.Float32.ceil x

let floor x = Smtml.Typed.Float32.floor x

let trunc x = Smtml.Typed.Float32.trunc x

let nearest x = Smtml.Typed.Float32.nearest x

let add x y = Smtml.Typed.Float32.add x y

let sub x y = Smtml.Typed.Float32.sub x y

let mul x y = Smtml.Typed.Float32.mul x y

let div x y = Smtml.Typed.Float32.div x y

let min x y = Smtml.Typed.Float32.min x y

let max x y = Smtml.Typed.Float32.max x y

let copy_sign x y = Smtml.Typed.Float32.copysign x y

let eq x y = Smtml.Typed.Float32.eq x y

let ne x y = Smtml.Typed.Float32.ne x y

let lt x y = Smtml.Typed.Float32.lt x y

let gt x y = Smtml.Typed.Float32.gt x y

let le x y = Smtml.Typed.Float32.le x y

let ge x y = Smtml.Typed.Float32.ge x y

let convert_i32_s x = Smtml.Typed.Float32.convert_i32_s x

let convert_i32_u x = Smtml.Typed.Float32.convert_i32_u x

let convert_i64_s x = Smtml.Typed.Float32.convert_i64_s x

let convert_i64_u x = Smtml.Typed.Float32.convert_i64_u x

let demote_f64 x = Smtml.Typed.Float32.demote_f64 x

let reinterpret_i32 x = Smtml.Typed.Float32.reinterpret_i32 x

let of_bits x = Smtml.Typed.Float32.reinterpret_i32 x

let to_bits x = Smtml.Typed.Float32.to_bv x

let pp fmt x = Smtml.Typed.Float32.pp fmt x
