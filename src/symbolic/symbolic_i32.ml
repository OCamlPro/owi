type t = Smtml.Expr.t

open Smtml.Expr

let ty = Smtml.Ty.Ty_bitv 32

let of_concrete (i : Int32.t) : t = value (Bitv (Smtml.Bitvector.of_int32 i))

let of_int (i : int) : t = of_concrete (Int32.of_int i)

let zero = of_concrete 0l

let one = of_concrete 1l

let to_boolean (e : t) : Symbolic_boolean.t =
  match view e with
  | Val (Bitv bv) ->
    if Smtml.Bitvector.eqz bv then Symbolic_boolean.false_
    else Symbolic_boolean.true_
  | Ptr _ -> Symbolic_boolean.true_
  | Symbol { ty = Ty_bool; _ } -> Symbolic_boolean.of_expr e
  | Cvtop (_, OfBool, cond) -> Symbolic_boolean.of_expr cond
  | _ ->
    let e = cvtop ty ToBool e in
    Symbolic_boolean.of_expr e

let of_boolean (e : Symbolic_boolean.t) : t =
  let e = Symbolic_boolean.to_expr e in
  match view e with
  | Val True -> one
  | Val False -> zero
  | Cvtop (Ty_bitv 32, ToBool, e') -> e'
  | _ -> cvtop (Ty_bitv 32) OfBool e

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

let boolify e =
  match view e with
  | Val (Bitv bv) when Smtml.Bitvector.eqz bv -> Some Symbolic_boolean.false_
  | Val (Bitv bv) when Smtml.Bitvector.eq_one bv -> Some Symbolic_boolean.true_
  | Cvtop (_, OfBool, cond) -> Some (Symbolic_boolean.of_expr cond)
  | _ -> None

let logand e1 e2 =
  match (boolify e1, boolify e2) with
  | Some b1, Some b2 -> of_boolean (Symbolic_boolean.and_ b1 b2)
  | _ -> binop ty And e1 e2

let logor e1 e2 =
  match (boolify e1, boolify e2) with
  | Some b1, Some b2 -> of_boolean (Symbolic_boolean.or_ b1 b2)
  | _ -> binop ty Or e1 e2

let logxor e1 e2 = binop ty Xor e1 e2

let shl e1 e2 = binop ty Shl e1 e2

let shr_s e1 e2 = binop ty ShrA e1 e2

let shr_u e1 e2 = binop ty ShrL e1 e2

let rotl e1 e2 = binop ty Rotl e1 e2

let rotr e1 e2 = binop ty Rotr e1 e2

let eq_concrete (e : t) (c : Int32.t) : Symbolic_boolean.t =
  relop Ty_bool Eq e (of_concrete c) |> Symbolic_boolean.of_expr

(* TODO: why do we use `Ty_bool` in the first two cases and `ty` in the others? What is the righe choice? *)
let eq e1 e2 : Symbolic_boolean.t =
  relop Ty_bool Eq e1 e2 |> Symbolic_boolean.of_expr

let ne e1 e2 : Symbolic_boolean.t =
  relop Ty_bool Ne e1 e2 |> Symbolic_boolean.of_expr

let lt e1 e2 : Symbolic_boolean.t =
  relop ty Lt e1 e2 |> Symbolic_boolean.of_expr

let gt e1 e2 : Symbolic_boolean.t =
  relop ty Gt e1 e2 |> Symbolic_boolean.of_expr

let lt_u e1 e2 : Symbolic_boolean.t =
  relop ty LtU e1 e2 |> Symbolic_boolean.of_expr

let gt_u e1 e2 : Symbolic_boolean.t =
  relop ty GtU e1 e2 |> Symbolic_boolean.of_expr

let le e1 e2 : Symbolic_boolean.t =
  relop ty Le e1 e2 |> Symbolic_boolean.of_expr

let ge e1 e2 : Symbolic_boolean.t =
  relop ty Ge e1 e2 |> Symbolic_boolean.of_expr

let le_u e1 e2 : Symbolic_boolean.t =
  relop ty LeU e1 e2 |> Symbolic_boolean.of_expr

let ge_u e1 e2 : Symbolic_boolean.t =
  relop ty GeU e1 e2 |> Symbolic_boolean.of_expr

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

let reinterpret_f32 x = cvtop ty Reinterpret_float x

let wrap_i64 x = cvtop ty WrapI64 x

(* FIXME: This is probably wrong? *)
let extend_s n x =
  cvtop ty (Sign_extend (32 - n)) (extract x ~high:(n / 8) ~low:0)

let pp = pp

let symbol s = Smtml.Expr.symbol s
