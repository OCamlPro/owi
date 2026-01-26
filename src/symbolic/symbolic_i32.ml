include Smtml.Typed.Bitv32

let to_boolean (e : t) : Symbolic_boolean.t =
  match Smtml.Typed.view e with
  | Val (Bitv bv) ->
    if Smtml.Bitvector.eqz bv then Symbolic_boolean.false_
    else Symbolic_boolean.true_
  | Ptr _ -> Symbolic_boolean.true_
  | Symbol { ty = Ty_bool; _ } -> Smtml.Typed.Bitv32.to_bool e
  | Cvtop (_, OfBool, cond) -> Smtml.Typed.Unsafe.wrap cond
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
  | Cvtop (_, OfBool, cond) -> Some (Smtml.Typed.Unsafe.wrap cond)
  | _ -> None

let logand e1 e2 =
  match (boolify e1, boolify e2) with
  | Some b1, Some b2 -> of_boolean (Symbolic_boolean.and_ b1 b2)
  | _ -> Smtml.Typed.Bitv32.logand e1 e2

let logor e1 e2 =
  match (boolify e1, boolify e2) with
  | Some b1, Some b2 -> of_boolean (Symbolic_boolean.or_ b1 b2)
  | _ -> Smtml.Typed.Bitv32.logor e1 e2

let eq_concrete (e : t) (c : Int32.t) : Symbolic_boolean.t =
  let c = of_int32 c in
  Smtml.Typed.Bitv32.eq c e
