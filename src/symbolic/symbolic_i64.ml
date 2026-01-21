type t = Smtml.Expr.t

let ty = Smtml.Ty.Ty_bitv 64

let of_concrete (i : Int64.t) : t =
  Smtml.Expr.value (Bitv (Smtml.Bitvector.of_int64 i))

let of_int (i : int) : t = of_concrete (Int64.of_int i)

let zero = of_concrete 0L

let clz e = Smtml.Expr.unop ty Clz e

let ctz e = Smtml.Expr.unop ty Ctz e

let popcnt e = Smtml.Expr.unop ty Popcnt e

let add e1 e2 = Smtml.Expr.binop ty Add e1 e2

let sub e1 e2 = Smtml.Expr.binop ty Sub e1 e2

let mul e1 e2 = Smtml.Expr.binop ty Mul e1 e2

let div e1 e2 = Smtml.Expr.binop ty Div e1 e2

let unsigned_div e1 e2 = Smtml.Expr.binop ty DivU e1 e2

let rem e1 e2 = Smtml.Expr.binop ty Rem e1 e2

let unsigned_rem e1 e2 = Smtml.Expr.binop ty RemU e1 e2

let logand e1 e2 = Smtml.Expr.binop ty And e1 e2

let logor e1 e2 = Smtml.Expr.binop ty Or e1 e2

let logxor e1 e2 = Smtml.Expr.binop ty Xor e1 e2

let shl e1 e2 = Smtml.Expr.binop ty Shl e1 e2

let shr_s e1 e2 = Smtml.Expr.binop ty ShrA e1 e2

let shr_u e1 e2 = Smtml.Expr.binop ty ShrL e1 e2

let rotl e1 e2 = Smtml.Expr.binop ty Rotl e1 e2

let rotr e1 e2 = Smtml.Expr.binop ty Rotr e1 e2

(* TODO: same as i32, why do we use `Ty_bool` sometimes and `ty` in other places? *)
let eq_concrete (e : t) (c : Int64.t) =
  Smtml.Expr.relop Ty_bool Eq e (of_concrete c) |> Symbolic_boolean.of_expr

let eq e1 e2 = Smtml.Expr.relop Ty_bool Eq e1 e2 |> Symbolic_boolean.of_expr

let ne e1 e2 = Smtml.Expr.relop Ty_bool Ne e1 e2 |> Symbolic_boolean.of_expr

let lt e1 e2 = Smtml.Expr.relop ty Lt e1 e2 |> Symbolic_boolean.of_expr

let gt e1 e2 = Smtml.Expr.relop ty Gt e1 e2 |> Symbolic_boolean.of_expr

let lt_u e1 e2 = Smtml.Expr.relop ty LtU e1 e2 |> Symbolic_boolean.of_expr

let gt_u e1 e2 = Smtml.Expr.relop ty GtU e1 e2 |> Symbolic_boolean.of_expr

let le e1 e2 = Smtml.Expr.relop ty Le e1 e2 |> Symbolic_boolean.of_expr

let ge e1 e2 = Smtml.Expr.relop ty Ge e1 e2 |> Symbolic_boolean.of_expr

let le_u e1 e2 = Smtml.Expr.relop ty LeU e1 e2 |> Symbolic_boolean.of_expr

let ge_u e1 e2 = Smtml.Expr.relop ty GeU e1 e2 |> Symbolic_boolean.of_expr

let of_int32 e = Smtml.Expr.cvtop ty (Sign_extend 32) e

let to_int32 e = Smtml.Expr.cvtop (Ty_bitv 32) WrapI64 e

let trunc_f32_s x =
  try Ok (Smtml.Expr.cvtop ty TruncSF32 (Smtml.Typed.raw x))
  with
  | Smtml.Eval.Eval_error ((`Integer_overflow | `Conversion_to_integer) as e) ->
    Error e

let trunc_f32_u x =
  try Ok (Smtml.Expr.cvtop ty TruncUF32 (Smtml.Typed.raw x))
  with
  | Smtml.Eval.Eval_error ((`Integer_overflow | `Conversion_to_integer) as e) ->
    Error e

let trunc_f64_s x =
  try Ok (Smtml.Expr.cvtop ty TruncSF64 (Smtml.Typed.raw x))
  with
  | Smtml.Eval.Eval_error ((`Integer_overflow | `Conversion_to_integer) as e) ->
    Error e

let trunc_f64_u x =
  try Ok (Smtml.Expr.cvtop ty TruncUF64 (Smtml.Typed.raw x))
  with
  | Smtml.Eval.Eval_error ((`Integer_overflow | `Conversion_to_integer) as e) ->
    Error e

let trunc_sat_f32_s x = Smtml.Expr.cvtop ty Trunc_sat_f32_s (Smtml.Typed.raw x)

let trunc_sat_f32_u x = Smtml.Expr.cvtop ty Trunc_sat_f32_u (Smtml.Typed.raw x)

let trunc_sat_f64_s x = Smtml.Expr.cvtop ty Trunc_sat_f64_s (Smtml.Typed.raw x)

let trunc_sat_f64_u x = Smtml.Expr.cvtop ty Trunc_sat_f64_u (Smtml.Typed.raw x)

let reinterpret_f64 x =
  Smtml.Expr.cvtop ty Reinterpret_float (Smtml.Typed.raw x)

(* FIXME: This is probably wrong? *)
let extend_s n x =
  Smtml.Expr.cvtop ty
    (Sign_extend (64 - n))
    (Smtml.Expr.extract x ~high:(n / 8) ~low:0)

let extend_i32_s x = Smtml.Expr.cvtop ty (Sign_extend 32) x

let extend_i32_u x = Smtml.Expr.cvtop ty (Zero_extend 32) x

let pp = Smtml.Expr.pp
