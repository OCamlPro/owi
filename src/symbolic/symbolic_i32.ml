type t = Smtml.Typed.bitv32 Smtml.Typed.t

let ty = Smtml.Ty.Ty_bitv 32

let of_concrete (i : Int32.t) : t =
  Smtml.Typed.Bitv32.v (Smtml.Bitvector.of_int32 i)

let of_int (i : int) : t = of_concrete (Int32.of_int i)

let zero = of_concrete 0l

let one = of_concrete 1l

let to_boolean (e : t) : Symbolic_boolean.t =
  match Smtml.Typed.view e with
  | Val (Bitv bv) ->
    if Smtml.Bitvector.eqz bv then Symbolic_boolean.false_
    else Symbolic_boolean.true_
  | Ptr _ -> Symbolic_boolean.true_
  | Symbol { ty = Ty_bool; _ } -> Symbolic_boolean.of_expr (Smtml.Typed.raw e)
  | Cvtop (_, OfBool, cond) -> Symbolic_boolean.of_expr cond
  | _ ->
    (* TODO: Smtml.Typed.Bitv32.to_bool e *)
    let e = Smtml.Expr.cvtop ty ToBool (Smtml.Typed.raw e) in
    Symbolic_boolean.of_expr e

let of_boolean (e : Symbolic_boolean.t) : t =
  match Smtml.Typed.view e with
  | Val True -> one
  | Val False -> zero
  | _ ->
    (* TODO: Smtml.Typed.Bitv32.of_bool e *)
    Smtml.Expr.cvtop (Ty_bitv 32) OfBool (Smtml.Typed.raw e)
    |> Smtml.Typed.unsafe

let clz e = Smtml.Expr.unop ty Clz (Smtml.Typed.raw e) |> Smtml.Typed.unsafe

let ctz e = Smtml.Expr.unop ty Ctz (Smtml.Typed.raw e) |> Smtml.Typed.unsafe

let popcnt e =
  Smtml.Expr.unop ty Popcnt (Smtml.Typed.raw e) |> Smtml.Typed.unsafe

let add e1 e2 = Smtml.Typed.Bitv32.add e1 e2

let sub e1 e2 = Smtml.Typed.Bitv32.sub e1 e2

let mul e1 e2 = Smtml.Typed.Bitv32.mul e1 e2

let div e1 e2 = Smtml.Typed.Bitv32.div e1 e2

let unsigned_div e1 e2 =
  Smtml.Expr.binop ty DivU (Smtml.Typed.raw e1) (Smtml.Typed.raw e2)
  |> Smtml.Typed.unsafe

let rem e1 e2 = Smtml.Typed.Bitv32.rem e1 e2

let unsigned_rem e1 e2 =
  Smtml.Expr.binop ty RemU (Smtml.Typed.raw e1) (Smtml.Typed.raw e2)
  |> Smtml.Typed.unsafe

let boolify e =
  match Smtml.Typed.view e with
  | Val (Bitv bv) when Smtml.Bitvector.eqz bv -> Some Symbolic_boolean.false_
  | Val (Bitv bv) when Smtml.Bitvector.eq_one bv -> Some Symbolic_boolean.true_
  | Cvtop (_, OfBool, cond) -> Some (Symbolic_boolean.of_expr cond)
  | _ -> None

let logand e1 e2 =
  match (boolify e1, boolify e2) with
  | Some b1, Some b2 -> of_boolean (Symbolic_boolean.and_ b1 b2)
  | _ -> Smtml.Typed.Bitv32.logand e1 e2

let logor e1 e2 =
  match (boolify e1, boolify e2) with
  | Some b1, Some b2 -> of_boolean (Symbolic_boolean.or_ b1 b2)
  | _ -> Smtml.Typed.Bitv32.logor e1 e2

let logxor e1 e2 = Smtml.Typed.Bitv32.logxor e1 e2

let shl e1 e2 = Smtml.Typed.Bitv32.shl e1 e2

let shr_s e1 e2 = Smtml.Typed.Bitv32.ashr e1 e2

let shr_u e1 e2 = Smtml.Typed.Bitv32.lshr e1 e2

let rotl e1 e2 = Smtml.Typed.Bitv32.rotate_left e1 e2

let rotr e1 e2 = Smtml.Typed.Bitv32.rotate_right e1 e2

let eq_concrete (e : t) (c : Int32.t) : Symbolic_boolean.t =
  let c = of_concrete c in
  Smtml.Typed.Bitv32.eq c e

let eq e1 e2 : Symbolic_boolean.t = Smtml.Typed.Bitv32.eq e1 e2

let ne e1 e2 : Symbolic_boolean.t =
  Smtml.Expr.relop Ty_bool Ne (Smtml.Typed.raw e1) (Smtml.Typed.raw e2)
  |> Symbolic_boolean.of_expr

let lt e1 e2 : Symbolic_boolean.t = Smtml.Typed.Bitv32.lt e1 e2

let gt e1 e2 : Symbolic_boolean.t = Smtml.Typed.Bitv32.gt e1 e2

let lt_u e1 e2 : Symbolic_boolean.t = Smtml.Typed.Bitv32.lt_u e1 e2

let gt_u e1 e2 : Symbolic_boolean.t = Smtml.Typed.Bitv32.gt_u e1 e2

let le e1 e2 : Symbolic_boolean.t = Smtml.Typed.Bitv32.le e1 e2

let ge e1 e2 : Symbolic_boolean.t = Smtml.Typed.Bitv32.ge e1 e2

let le_u e1 e2 : Symbolic_boolean.t = Smtml.Typed.Bitv32.le_u e1 e2

let ge_u e1 e2 : Symbolic_boolean.t = Smtml.Typed.Bitv32.ge_u e1 e2

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

let reinterpret_f32 x =
  Smtml.Expr.cvtop ty Reinterpret_float (Smtml.Typed.raw x)
  |> Smtml.Typed.unsafe

let wrap_i64 x =
  Smtml.Expr.cvtop ty WrapI64 (Smtml.Typed.raw x) |> Smtml.Typed.unsafe

(* FIXME: This is probably wrong? *)
let extend_s n x =
  Smtml.Expr.cvtop ty
    (Sign_extend (32 - n))
    (Smtml.Expr.extract (Smtml.Typed.raw x) ~high:(n / 8) ~low:0)
  |> Smtml.Typed.unsafe

let pp fmt x = Smtml.Typed.Bitv32.pp fmt x

let symbol s = Smtml.Expr.symbol s |> Smtml.Typed.unsafe
