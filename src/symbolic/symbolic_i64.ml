type t = Smtml.Expr.t

open Smtml.Expr

let ty = Smtml.Ty.Ty_bitv 64

let of_concrete (i : Int64.t) : t = value (Bitv (Smtml.Bitvector.of_int64 i))

let of_int (i : int) : t = of_concrete (Int64.of_int i)

let zero = of_concrete 0L

let clz e = unop ty Clz e

let ctz e = unop ty Ctz e

let popcnt e = unop ty Popcnt e

let add e1 e2 = binop ty Add e1 e2

let sub e1 e2 = binop ty Sub e1 e2

let mul e1 e2 = binop ty Mul e1 e2

let div e1 e2 = binop ty Div e1 e2

let unsigned_div e1 e2 = binop ty DivU e1 e2

let rem e1 e2 = binop ty Rem e1 e2

let unsigned_rem e1 e2 = binop ty RemU e1 e2

let logand e1 e2 = binop ty And e1 e2

let logor e1 e2 = binop ty Or e1 e2

let logxor e1 e2 = binop ty Xor e1 e2

let shl e1 e2 = binop ty Shl e1 e2

let shr_s e1 e2 = binop ty ShrA e1 e2

let shr_u e1 e2 = binop ty ShrL e1 e2

let rotl e1 e2 = binop ty Rotl e1 e2

let rotr e1 e2 = binop ty Rotr e1 e2

(* TODO: same as i32, why do we use `Ty_bool` sometimes and `ty` in other places? *)
let eq_concrete (e : t) (c : Int64.t) =
  relop Ty_bool Eq e (of_concrete c) |> Symbolic_boolean.of_expr

let eq e1 e2 = relop Ty_bool Eq e1 e2 |> Symbolic_boolean.of_expr

let ne e1 e2 = relop Ty_bool Ne e1 e2 |> Symbolic_boolean.of_expr

let lt e1 e2 = relop ty Lt e1 e2 |> Symbolic_boolean.of_expr

let gt e1 e2 = relop ty Gt e1 e2 |> Symbolic_boolean.of_expr

let lt_u e1 e2 = relop ty LtU e1 e2 |> Symbolic_boolean.of_expr

let gt_u e1 e2 = relop ty GtU e1 e2 |> Symbolic_boolean.of_expr

let le e1 e2 = relop ty Le e1 e2 |> Symbolic_boolean.of_expr

let ge e1 e2 = relop ty Ge e1 e2 |> Symbolic_boolean.of_expr

let le_u e1 e2 = relop ty LeU e1 e2 |> Symbolic_boolean.of_expr

let ge_u e1 e2 = relop ty GeU e1 e2 |> Symbolic_boolean.of_expr

let of_int32 e = cvtop ty (Sign_extend 32) e

let to_int32 e = cvtop (Ty_bitv 32) WrapI64 e

let trunc_f32_s x =
  try Ok (cvtop ty TruncSF32 x)
  with
  | Smtml.Eval.Eval_error ((`Integer_overflow | `Conversion_to_integer) as e) ->
    Error e

let trunc_f32_u x =
  try Ok (cvtop ty TruncUF32 x)
  with
  | Smtml.Eval.Eval_error ((`Integer_overflow | `Conversion_to_integer) as e) ->
    Error e

let trunc_f64_s x =
  try Ok (cvtop ty TruncSF64 x)
  with
  | Smtml.Eval.Eval_error ((`Integer_overflow | `Conversion_to_integer) as e) ->
    Error e

let trunc_f64_u x =
  try Ok (cvtop ty TruncUF64 x)
  with
  | Smtml.Eval.Eval_error ((`Integer_overflow | `Conversion_to_integer) as e) ->
    Error e

let trunc_sat_f32_s x = cvtop ty Trunc_sat_f32_s x

let trunc_sat_f32_u x = cvtop ty Trunc_sat_f32_u x

let trunc_sat_f64_s x = cvtop ty Trunc_sat_f64_s x

let trunc_sat_f64_u x = cvtop ty Trunc_sat_f64_u x

let reinterpret_f64 x = cvtop ty Reinterpret_float x

(* FIXME: This is probably wrong? *)
let extend_s n x =
  cvtop ty (Sign_extend (64 - n)) (extract x ~high:(n / 8) ~low:0)

let extend_i32_s x = cvtop ty (Sign_extend 32) x

let extend_i32_u x = cvtop ty (Zero_extend 32) x

let pp = pp
