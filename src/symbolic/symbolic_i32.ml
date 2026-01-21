type t = Smtml.Expr.t

let ty = Smtml.Ty.Ty_bitv 32

let of_concrete (i : Int32.t) : t =
  Smtml.Expr.value (Bitv (Smtml.Bitvector.of_int32 i))

let of_int (i : int) : t = of_concrete (Int32.of_int i)

let zero = of_concrete 0l

let one = of_concrete 1l

let to_boolean (e : t) : Symbolic_boolean.t =
  match Smtml.Expr.view e with
  | Val (Bitv bv) ->
    if Smtml.Bitvector.eqz bv then Symbolic_boolean.false_
    else Symbolic_boolean.true_
  | Ptr _ -> Symbolic_boolean.true_
  | Symbol { ty = Ty_bool; _ } -> Symbolic_boolean.of_expr e
  | Cvtop (_, OfBool, cond) -> Symbolic_boolean.of_expr cond
  | _ ->
    let e = Smtml.Expr.cvtop ty ToBool e in
    Symbolic_boolean.of_expr e

let of_boolean (e : Symbolic_boolean.t) : t =
  let e = Symbolic_boolean.to_expr e in
  match Smtml.Expr.view e with
  | Val True -> one
  | Val False -> zero
  | Cvtop (Ty_bitv 32, ToBool, e') -> e'
  | _ -> Smtml.Expr.cvtop (Ty_bitv 32) OfBool e

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

let boolify e =
  match Smtml.Expr.view e with
  | Val (Bitv bv) when Smtml.Bitvector.eqz bv -> Some Symbolic_boolean.false_
  | Val (Bitv bv) when Smtml.Bitvector.eq_one bv -> Some Symbolic_boolean.true_
  | Cvtop (_, OfBool, cond) -> Some (Symbolic_boolean.of_expr cond)
  | _ -> None

let logand e1 e2 =
  match (boolify e1, boolify e2) with
  | Some b1, Some b2 -> of_boolean (Symbolic_boolean.and_ b1 b2)
  | _ -> Smtml.Expr.binop ty And e1 e2

let logor e1 e2 =
  match (boolify e1, boolify e2) with
  | Some b1, Some b2 -> of_boolean (Symbolic_boolean.or_ b1 b2)
  | _ -> Smtml.Expr.binop ty Or e1 e2

let logxor e1 e2 = Smtml.Expr.binop ty Xor e1 e2

let shl e1 e2 = Smtml.Expr.binop ty Shl e1 e2

let shr_s e1 e2 = Smtml.Expr.binop ty ShrA e1 e2

let shr_u e1 e2 = Smtml.Expr.binop ty ShrL e1 e2

let rotl e1 e2 = Smtml.Expr.binop ty Rotl e1 e2

let rotr e1 e2 = Smtml.Expr.binop ty Rotr e1 e2

let eq_concrete (e : t) (c : Int32.t) : Symbolic_boolean.t =
  Smtml.Expr.relop Ty_bool Eq e (of_concrete c) |> Symbolic_boolean.of_expr

(* TODO: why do we use `Ty_bool` in the first two cases and `ty` in the others? What is the righe choice? *)
let eq e1 e2 : Symbolic_boolean.t =
  Smtml.Expr.relop Ty_bool Eq e1 e2 |> Symbolic_boolean.of_expr

let ne e1 e2 : Symbolic_boolean.t =
  Smtml.Expr.relop Ty_bool Ne e1 e2 |> Symbolic_boolean.of_expr

let lt e1 e2 : Symbolic_boolean.t =
  Smtml.Expr.relop ty Lt e1 e2 |> Symbolic_boolean.of_expr

let gt e1 e2 : Symbolic_boolean.t =
  Smtml.Expr.relop ty Gt e1 e2 |> Symbolic_boolean.of_expr

let lt_u e1 e2 : Symbolic_boolean.t =
  Smtml.Expr.relop ty LtU e1 e2 |> Symbolic_boolean.of_expr

let gt_u e1 e2 : Symbolic_boolean.t =
  Smtml.Expr.relop ty GtU e1 e2 |> Symbolic_boolean.of_expr

let le e1 e2 : Symbolic_boolean.t =
  Smtml.Expr.relop ty Le e1 e2 |> Symbolic_boolean.of_expr

let ge e1 e2 : Symbolic_boolean.t =
  Smtml.Expr.relop ty Ge e1 e2 |> Symbolic_boolean.of_expr

let le_u e1 e2 : Symbolic_boolean.t =
  Smtml.Expr.relop ty LeU e1 e2 |> Symbolic_boolean.of_expr

let ge_u e1 e2 : Symbolic_boolean.t =
  Smtml.Expr.relop ty GeU e1 e2 |> Symbolic_boolean.of_expr

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

let reinterpret_f32 x =
  Smtml.Expr.cvtop ty Reinterpret_float (Smtml.Typed.raw x)

let wrap_i64 x = Smtml.Expr.cvtop ty WrapI64 x

(* FIXME: This is probably wrong? *)
let extend_s n x =
  Smtml.Expr.cvtop ty
    (Sign_extend (32 - n))
    (Smtml.Expr.extract x ~high:(n / 8) ~low:0)

let pp = Smtml.Expr.pp

let symbol s = Smtml.Expr.symbol s
