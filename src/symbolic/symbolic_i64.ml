include Smtml.Typed.Bitv64

type t = Smtml.Typed.bitv64 Smtml.Typed.t

let ty = Smtml.Ty.Ty_bitv 64

let of_concrete (i : Int64.t) : t =
  Smtml.Typed.Bitv64.v (Smtml.Bitvector.of_int64 i)

let of_int (i : int) : t = of_concrete (Int64.of_int i)

let zero = of_concrete 0L

let shr_s e1 e2 = Smtml.Typed.Bitv64.ashr e1 e2

let shr_u e1 e2 = Smtml.Typed.Bitv64.lshr e1 e2

let rotl e1 e2 = Smtml.Typed.Bitv64.rotate_left e1 e2

let rotr e1 e2 = Smtml.Typed.Bitv64.rotate_right e1 e2

let eq_concrete (e : t) (c : Int64.t) : Symbolic_boolean.t =
  let c = of_concrete c in
  Smtml.Typed.Bitv64.eq c e

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
