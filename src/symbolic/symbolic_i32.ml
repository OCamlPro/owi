include Smtml.Typed.Bitv32

type t = Smtml.Typed.bitv32 Smtml.Typed.t

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
  | Symbol { ty = Ty_bool; _ } -> Smtml.Typed.Bitv32.to_bool e
  | Cvtop (_, OfBool, cond) -> Smtml.Typed.unsafe cond
  | _ -> Smtml.Typed.Bitv32.to_bool e

let of_boolean (e : Symbolic_boolean.t) : t =
  match Smtml.Typed.view e with
  | Val True -> one
  | Val False -> zero
  | _ -> Smtml.Typed.Bitv32.of_bool e

let boolify e =
  match Smtml.Typed.view e with
  | Val (Bitv bv) when Smtml.Bitvector.eqz bv -> Some Symbolic_boolean.false_
  | Val (Bitv bv) when Smtml.Bitvector.eq_one bv -> Some Symbolic_boolean.true_
  | Cvtop (_, OfBool, cond) -> Some (Smtml.Typed.unsafe cond)
  | _ -> None

let logand e1 e2 =
  match (boolify e1, boolify e2) with
  | Some b1, Some b2 -> of_boolean (Symbolic_boolean.and_ b1 b2)
  | _ -> Smtml.Typed.Bitv32.logand e1 e2

let logor e1 e2 =
  match (boolify e1, boolify e2) with
  | Some b1, Some b2 -> of_boolean (Symbolic_boolean.or_ b1 b2)
  | _ -> Smtml.Typed.Bitv32.logor e1 e2

let shr_s e1 e2 = Smtml.Typed.Bitv32.ashr e1 e2

let shr_u e1 e2 = Smtml.Typed.Bitv32.lshr e1 e2

let rotl e1 e2 = Smtml.Typed.Bitv32.rotate_left e1 e2

let rotr e1 e2 = Smtml.Typed.Bitv32.rotate_right e1 e2

let eq_concrete (e : t) (c : Int32.t) : Symbolic_boolean.t =
  let c = of_concrete c in
  Smtml.Typed.Bitv32.eq c e

let trunc_f32_s x =
  try Ok (Smtml.Typed.Bitv32.trunc_f32_s x)
  with
  | Smtml.Eval.Eval_error ((`Integer_overflow | `Conversion_to_integer) as e) ->
    Error e

let trunc_f32_u x =
  try Ok (Smtml.Typed.Bitv32.trunc_f32_u x)
  with
  | Smtml.Eval.Eval_error ((`Integer_overflow | `Conversion_to_integer) as e) ->
    Error e

let trunc_f64_s x =
  try Ok (Smtml.Typed.Bitv32.trunc_f64_s x)
  with
  | Smtml.Eval.Eval_error ((`Integer_overflow | `Conversion_to_integer) as e) ->
    Error e

let trunc_f64_u x =
  try Ok (Smtml.Typed.Bitv32.trunc_f64_u x)
  with
  | Smtml.Eval.Eval_error ((`Integer_overflow | `Conversion_to_integer) as e) ->
    Error e
