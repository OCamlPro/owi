type t = Smtml.Expr.t

open Smtml.Expr

let ty = Smtml.Ty.Ty_bitv 32

let of_concrete (i : Int32.t) : t = value (Bitv (Smtml.Bitvector.of_int32 i))

let of_int (i : int) : t = of_concrete (Int32.of_int i)

let to_bool (e : t) : Symbolic_boolean.t =
  match view e with
  | Val (Bitv bv) ->
    if Smtml.Bitvector.eqz bv then Symbolic_boolean.false_
    else Symbolic_boolean.true_
  | Ptr _ -> Symbolic_boolean.true_
  | Symbol { ty = Ty_bool; _ } -> e
  | Cvtop (_, OfBool, cond) -> cond
  | _ -> cvtop ty ToBool e

let zero = of_concrete 0l

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
  | Cvtop (_, OfBool, cond) -> Some cond
  | _ -> None

let logand e1 e2 =
  match (boolify e1, boolify e2) with
  | Some b1, Some b2 -> Symbolic_boolean.to_i32 (Bool.and_ b1 b2)
  | _ -> binop ty And e1 e2

let logor e1 e2 =
  match (boolify e1, boolify e2) with
  | Some b1, Some b2 -> Symbolic_boolean.to_i32 (Bool.or_ b1 b2)
  | _ -> binop ty Or e1 e2

let logxor e1 e2 = binop ty Xor e1 e2

let shl e1 e2 = binop ty Shl e1 e2

let shr_s e1 e2 = binop ty ShrA e1 e2

let shr_u e1 e2 = binop ty ShrL e1 e2

let rotl e1 e2 = binop ty Rotl e1 e2

let rotr e1 e2 = binop ty Rotr e1 e2

let eq_concrete e c =
  match view e with
  | Cvtop (_, OfBool, cond) -> begin
    match c with
    | 0l -> Bool.not cond
    | 1l -> cond
    | _ -> Symbolic_boolean.false_
  end
  | _ -> relop Ty_bool Eq e (of_concrete c)

let eq e1 e2 =
  if phys_equal e1 e2 then Symbolic_boolean.true_ else relop Ty_bool Eq e1 e2

let ne e1 e2 =
  if phys_equal e1 e2 then Symbolic_boolean.false_ else relop Ty_bool Ne e1 e2

let lt e1 e2 =
  if phys_equal e1 e2 then Symbolic_boolean.false_ else relop ty Lt e1 e2

let gt e1 e2 =
  if phys_equal e1 e2 then Symbolic_boolean.false_ else relop ty Gt e1 e2

let lt_u e1 e2 =
  if phys_equal e1 e2 then Symbolic_boolean.false_ else relop ty LtU e1 e2

let gt_u e1 e2 =
  if phys_equal e1 e2 then Symbolic_boolean.false_ else relop ty GtU e1 e2

let le e1 e2 =
  if phys_equal e1 e2 then Symbolic_boolean.true_ else relop ty Le e1 e2

let ge e1 e2 =
  if phys_equal e1 e2 then Symbolic_boolean.true_ else relop ty Ge e1 e2

let le_u e1 e2 =
  if phys_equal e1 e2 then Symbolic_boolean.true_ else relop ty LeU e1 e2

let ge_u e1 e2 =
  if phys_equal e1 e2 then Symbolic_boolean.true_ else relop ty GeU e1 e2

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
