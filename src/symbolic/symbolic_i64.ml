include Smtml.Typed.Bitv64

type t = Smtml.Typed.bitv64 Smtml.Typed.t

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

let trunc_f32_s x =
  try Ok (Smtml.Typed.Bitv64.trunc_f32_s x)
  with
  | Smtml.Eval.Eval_error ((`Integer_overflow | `Conversion_to_integer) as e) ->
    Error e

let trunc_f32_u x =
  try Ok (Smtml.Typed.Bitv64.trunc_f32_u x)
  with
  | Smtml.Eval.Eval_error ((`Integer_overflow | `Conversion_to_integer) as e) ->
    Error e

let trunc_f64_s x =
  try Ok (Smtml.Typed.Bitv64.trunc_f64_s x)
  with
  | Smtml.Eval.Eval_error ((`Integer_overflow | `Conversion_to_integer) as e) ->
    Error e

let trunc_f64_u x =
  try Ok (Smtml.Typed.Bitv64.trunc_f64_u x)
  with
  | Smtml.Eval.Eval_error ((`Integer_overflow | `Conversion_to_integer) as e) ->
    Error e
