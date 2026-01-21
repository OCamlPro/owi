type t = Smtml.Typed.float32 Smtml.Typed.t

let ty = Smtml.Ty.Ty_fp 32

let of_concrete (f : Float32.t) : t = Smtml.Typed.Float32.v (Float32.to_bits f)

let zero = of_concrete Float32.zero

let abs x = Smtml.Typed.Float32.abs x

let neg x = Smtml.Typed.Float32.neg x

let sqrt x = Smtml.Typed.Float32.sqrt x

let ceil x = Smtml.Expr.unop ty Ceil (Smtml.Typed.raw x) |> Smtml.Typed.unsafe

let floor x = Smtml.Expr.unop ty Floor (Smtml.Typed.raw x) |> Smtml.Typed.unsafe

let trunc x = Smtml.Expr.unop ty Trunc (Smtml.Typed.raw x) |> Smtml.Typed.unsafe

let nearest x =
  Smtml.Expr.unop ty Nearest (Smtml.Typed.raw x) |> Smtml.Typed.unsafe

let add x y = Smtml.Typed.Float32.add x y

let sub x y = Smtml.Typed.Float32.sub x y

let mul x y = Smtml.Typed.Float32.mul x y

let div x y = Smtml.Typed.Float32.div x y

let min x y = Smtml.Typed.Float32.min x y

let max x y = Smtml.Typed.Float32.max x y

let copy_sign x y =
  Smtml.Expr.binop ty Copysign (Smtml.Typed.raw x) (Smtml.Typed.raw y)
  |> Smtml.Typed.unsafe

let eq x y = Smtml.Typed.Float32.eq x y

let ne x y =
  Smtml.Expr.relop ty Ne (Smtml.Typed.raw x) (Smtml.Typed.raw y)
  |> Smtml.Typed.unsafe

let lt x y = Smtml.Typed.Float32.lt x y

let gt x y = Smtml.Typed.Float32.gt x y

let le x y = Smtml.Typed.Float32.le x y

let ge x y = Smtml.Typed.Float32.ge x y

let convert_i32_s x = Smtml.Expr.cvtop ty ConvertSI32 x |> Smtml.Typed.unsafe

let convert_i32_u x = Smtml.Expr.cvtop ty ConvertUI32 x |> Smtml.Typed.unsafe

let convert_i64_s x = Smtml.Expr.cvtop ty ConvertSI64 x |> Smtml.Typed.unsafe

let convert_i64_u x = Smtml.Expr.cvtop ty ConvertUI64 x |> Smtml.Typed.unsafe

let demote_f64 x =
  Smtml.Expr.cvtop ty DemoteF64 (Smtml.Typed.raw x) |> Smtml.Typed.unsafe

let reinterpret_i32 x =
  Smtml.Expr.cvtop ty Reinterpret_int x |> Smtml.Typed.unsafe

let of_bits x = Smtml.Expr.cvtop ty Reinterpret_int x |> Smtml.Typed.unsafe

let to_bits x =
  Smtml.Expr.cvtop (Ty_bitv 32) Reinterpret_float (Smtml.Typed.raw x)

let pp fmt x = Smtml.Expr.pp fmt (Smtml.Typed.raw x)
