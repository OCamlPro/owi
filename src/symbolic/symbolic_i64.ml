type t = Smtml.Typed.bitv64 Smtml.Typed.t

let ty = Smtml.Ty.Ty_bitv 64

let of_concrete (i : Int64.t) : t =
  Smtml.Typed.Bitv64.v (Smtml.Bitvector.of_int64 i)

let of_int (i : int) : t = of_concrete (Int64.of_int i)

let zero = of_concrete 0L

let clz e = Smtml.Expr.unop ty Clz (Smtml.Typed.raw e) |> Smtml.Typed.unsafe

let ctz e = Smtml.Expr.unop ty Ctz (Smtml.Typed.raw e) |> Smtml.Typed.unsafe

let popcnt e =
  Smtml.Expr.unop ty Popcnt (Smtml.Typed.raw e) |> Smtml.Typed.unsafe

let add e1 e2 = Smtml.Typed.Bitv64.add e1 e2

let sub e1 e2 = Smtml.Typed.Bitv64.sub e1 e2

let mul e1 e2 = Smtml.Typed.Bitv64.mul e1 e2

let div e1 e2 = Smtml.Typed.Bitv64.div e1 e2

let unsigned_div e1 e2 =
  Smtml.Expr.binop ty DivU (Smtml.Typed.raw e1) (Smtml.Typed.raw e2)
  |> Smtml.Typed.unsafe

let rem e1 e2 = Smtml.Typed.Bitv64.rem e1 e2

let unsigned_rem e1 e2 =
  Smtml.Expr.binop ty RemU (Smtml.Typed.raw e1) (Smtml.Typed.raw e2)
  |> Smtml.Typed.unsafe

let logand e1 e2 = Smtml.Typed.Bitv64.logand e1 e2

let logor e1 e2 = Smtml.Typed.Bitv64.logor e1 e2

let logxor e1 e2 = Smtml.Typed.Bitv64.logxor e1 e2

let shl e1 e2 = Smtml.Typed.Bitv64.shl e1 e2

let shr_s e1 e2 = Smtml.Typed.Bitv64.ashr e1 e2

let shr_u e1 e2 = Smtml.Typed.Bitv64.lshr e1 e2

let rotl e1 e2 = Smtml.Typed.Bitv64.rotate_left e1 e2

let rotr e1 e2 = Smtml.Typed.Bitv64.rotate_right e1 e2

let eq_concrete (e : t) (c : Int64.t) : Symbolic_boolean.t =
  let c = of_concrete c in
  Smtml.Typed.Bitv64.eq c e

let eq e1 e2 : Symbolic_boolean.t = Smtml.Typed.Bitv64.eq e1 e2

let ne e1 e2 : Symbolic_boolean.t =
  Smtml.Expr.relop Ty_bool Ne (Smtml.Typed.raw e1) (Smtml.Typed.raw e2)
  |> Symbolic_boolean.of_expr

let lt e1 e2 : Symbolic_boolean.t = Smtml.Typed.Bitv64.lt e1 e2

let gt e1 e2 : Symbolic_boolean.t = Smtml.Typed.Bitv64.gt e1 e2

let lt_u e1 e2 : Symbolic_boolean.t = Smtml.Typed.Bitv64.lt_u e1 e2

let gt_u e1 e2 : Symbolic_boolean.t = Smtml.Typed.Bitv64.gt_u e1 e2

let le e1 e2 : Symbolic_boolean.t = Smtml.Typed.Bitv64.le e1 e2

let ge e1 e2 : Symbolic_boolean.t = Smtml.Typed.Bitv64.ge e1 e2

let le_u e1 e2 : Symbolic_boolean.t = Smtml.Typed.Bitv64.le_u e1 e2

let ge_u e1 e2 : Symbolic_boolean.t = Smtml.Typed.Bitv64.ge_u e1 e2

let of_int32 e =
  Smtml.Expr.cvtop ty (Sign_extend 32) (Smtml.Typed.raw e) |> Smtml.Typed.unsafe

let to_int32 e =
  Smtml.Expr.cvtop (Ty_bitv 32) WrapI64 (Smtml.Typed.raw e)
  |> Smtml.Typed.unsafe

let trunc_f32_s x =
  try
    Ok (Smtml.Expr.cvtop ty TruncSF32 (Smtml.Typed.raw x) |> Smtml.Typed.unsafe)
  with
  | Smtml.Eval.Eval_error ((`Integer_overflow | `Conversion_to_integer) as e) ->
    Error e

let trunc_f32_u x =
  try
    Ok (Smtml.Expr.cvtop ty TruncUF32 (Smtml.Typed.raw x) |> Smtml.Typed.unsafe)
  with
  | Smtml.Eval.Eval_error ((`Integer_overflow | `Conversion_to_integer) as e) ->
    Error e

let trunc_f64_s x =
  try
    Ok (Smtml.Expr.cvtop ty TruncSF64 (Smtml.Typed.raw x) |> Smtml.Typed.unsafe)
  with
  | Smtml.Eval.Eval_error ((`Integer_overflow | `Conversion_to_integer) as e) ->
    Error e

let trunc_f64_u x =
  try
    Ok (Smtml.Expr.cvtop ty TruncUF64 (Smtml.Typed.raw x) |> Smtml.Typed.unsafe)
  with
  | Smtml.Eval.Eval_error ((`Integer_overflow | `Conversion_to_integer) as e) ->
    Error e

let trunc_sat_f32_s x =
  Smtml.Expr.cvtop ty Trunc_sat_f32_s (Smtml.Typed.raw x) |> Smtml.Typed.unsafe

let trunc_sat_f32_u x =
  Smtml.Expr.cvtop ty Trunc_sat_f32_u (Smtml.Typed.raw x) |> Smtml.Typed.unsafe

let trunc_sat_f64_s x =
  Smtml.Expr.cvtop ty Trunc_sat_f64_s (Smtml.Typed.raw x) |> Smtml.Typed.unsafe

let trunc_sat_f64_u x =
  Smtml.Expr.cvtop ty Trunc_sat_f64_u (Smtml.Typed.raw x) |> Smtml.Typed.unsafe

let reinterpret_f64 x =
  Smtml.Expr.cvtop ty Reinterpret_float (Smtml.Typed.raw x)
  |> Smtml.Typed.unsafe

(* FIXME: This is probably wrong? *)
let extend_s n x =
  Smtml.Expr.cvtop ty
    (Sign_extend (64 - n))
    (Smtml.Expr.extract (Smtml.Typed.raw x) ~high:(n / 8) ~low:0)
  |> Smtml.Typed.unsafe

let extend_i32_s x =
  Smtml.Expr.cvtop ty (Sign_extend 32) (Smtml.Typed.raw x) |> Smtml.Typed.unsafe

let extend_i32_u x =
  Smtml.Expr.cvtop ty (Zero_extend 32) (Smtml.Typed.raw x) |> Smtml.Typed.unsafe

let pp fmt x = Smtml.Typed.Bitv64.pp fmt x
