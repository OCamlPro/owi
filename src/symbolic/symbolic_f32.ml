type t = Smtml.Expr.t

open Smtml.Expr

let ty = Smtml.Ty.Ty_fp 32

let of_concrete (f : Float32.t) : t = value (Num (F32 (Float32.to_bits f)))

let zero = of_concrete Float32.zero

let abs x = unop ty Abs x

let neg x = unop ty Neg x

let sqrt x = unop ty Sqrt x

let ceil x = unop ty Ceil x

let floor x = unop ty Floor x

let trunc x = unop ty Trunc x

let nearest x = unop ty Nearest x

let add x y = binop ty Add x y

let sub x y = binop ty Sub x y

let mul x y = binop ty Mul x y

let div x y = binop ty Div x y

let min x y = binop ty Min x y

let max x y = binop ty Max x y

let copy_sign x y = binop ty Copysign x y

let eq x y = relop ty Eq x y |> Symbolic_boolean.of_expr

let ne x y = relop ty Ne x y |> Symbolic_boolean.of_expr

let lt x y = relop ty Lt x y |> Symbolic_boolean.of_expr

let gt x y = relop ty Gt x y |> Symbolic_boolean.of_expr

let le x y = relop ty Le x y |> Symbolic_boolean.of_expr

let ge x y = relop ty Ge x y |> Symbolic_boolean.of_expr

let convert_i32_s x = cvtop ty ConvertSI32 x

let convert_i32_u x = cvtop ty ConvertUI32 x

let convert_i64_s x = cvtop ty ConvertSI64 x

let convert_i64_u x = cvtop ty ConvertUI64 x

let demote_f64 x = cvtop ty DemoteF64 x

let reinterpret_i32 x = cvtop ty Reinterpret_int x

let of_bits x = cvtop ty Reinterpret_int x

let to_bits x = cvtop (Ty_bitv 32) Reinterpret_float x

let pp = pp
