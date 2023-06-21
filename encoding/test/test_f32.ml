open Encoding

let solver = Batch.create ()
let x = Expression.mk_symbol_s `F32Type "x"
let nan = FloatingPoint.mk_val Float.nan `F32Type

let%test "deterministic_nan" =
  let pc =
    [
      Boolean.mk_not (FloatingPoint.mk_is_nan x `F32Type);
      FloatingPoint.mk_is_nan x `F32Type;
    ]
  in
  false = Batch.check_sat solver pc

let%test "nondeterministic_nan" =
  let pc =
    [ FloatingPoint.mk_ne x nan `F32Type; FloatingPoint.mk_is_nan x `F32Type ]
  in
  true = Batch.check_sat solver pc
