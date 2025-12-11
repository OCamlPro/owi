type t = Smtml.Expr.t

open Smtml.Expr

let false_ = Bool.false_

let false_i32 = value (Bitv (Smtml.Bitvector.of_int32 0l))

let true_ = Bool.true_

let true_i32 = value (Bitv (Smtml.Bitvector.of_int32 1l))

let of_concrete (i : bool) : t = if i then true_ else false_

let not e = Bool.not e

let or_ e1 e2 = Bool.or_ e1 e2

let and_ e1 e2 = Bool.and_ e1 e2

let to_i32 e =
  match view e with
  | Val True -> true_i32
  | Val False -> false_i32
  | Cvtop (Ty_bitv 32, ToBool, e') -> e'
  | _ -> cvtop (Ty_bitv 32) OfBool e

let ite c ~if_true ~if_false = Bool.ite c if_true if_false

let pp = pp
