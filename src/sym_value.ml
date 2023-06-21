module Symbolic = struct
  open Encoding 

  module Expr = Expression

  type vbool = Expr.t

  type int32 = Expr.t

  type int64 = Expr.t

  type float32 = Expr.t

  type float64 = Expr.t

  type 'a ref_value = unit

  type 'a t =
    | I32 of int32
    | I64 of int64
    | F32 of float32
    | F64 of float64
    | Ref of 'a ref_value

  let mk_i32 x = Expr.Val (Value.Num (Types.I32 x))
  let mk_i64 x = Expr.Val (Value.Num (Types.I64 x))
  let mk_f32 x = Expr.Val (Value.Num (Types.F32 x))
  let mk_f64 x = Expr.Val (Value.Num (Types.F64 x))

  let const_i32 (i : Int32.t) : int32 = mk_i32 i

  let const_i64 (i : Int64.t) : int64 = mk_i64 i

  let const_f32 (f : Float32.t) : float32 = mk_f32 (Float32.to_bits f)

  let const_f64 (f : Float64.t) : float64 = mk_f64 (Float64.to_bits f)

  let ref_null _ = assert false

  let pp _ _ = failwith "TODO"

  module Bool = struct
    let not = Boolean.mk_not 
    let int32 v = Boolean.mk_ite v (mk_i32 0l) (mk_i32 1l)
    let or_ = Boolean.mk_or
    let and_ = Boolean.mk_and
  end

  module I32 = struct
    type num = Expr.t
    type vbool = Expr.t
    type const = Int64.t

    let zero = mk_i32 0l

    let clz e = BitVector.mk_clz e `I32Type

    let ctz _ = failwith "i32_ctz: TODO"

    let popcnt _ = failwith "i32_popcnt: TODO"

    let add e1 e2 = BitVector.mk_add e1 e2 `I32Type

    let sub e1 e2 = BitVector.mk_sub e1 e2 `I32Type

    let mul e1 e2 = BitVector.mk_mul e1 e2 `I32Type

    let div e1 e2 = BitVector.mk_div_s e1 e2 `I32Type

    let unsigned_div e1 e2 = BitVector.mk_div_u e1 e2 `I32Type

    let rem e1 e2 = BitVector.mk_rem_s e1 e2 `I32Type

    let unsigned_rem e1 e2 = BitVector.mk_rem_u e1 e2 `I32Type

    let logand e1 e2 = BitVector.mk_and e1 e2 `I32Type

    let logor e1 e2 = BitVector.mk_or e1 e2 `I32Type

    let logxor e1 e2 = BitVector.mk_xor e1 e2 `I32Type

    let shl e1 e2 = BitVector.mk_shl e1 e2 `I32Type

    let shr_s e1 e2 = BitVector.mk_shr_s e1 e2 `I32Type

    let shr_u e1 e2 = BitVector.mk_shr_u e1 e2 `I32Type

    let rotl e1 e2 = BitVector.mk_rotl e1 e2 `I32Type

    let rotr e1 e2 = BitVector.mk_rotr e1 e2 `I32Type

    let eq_const e c = BitVector.mk_eq e (mk_i32 c) `I32Type

    let eq e1 e2 = BitVector.mk_eq e1 e2 `I32Type

    let ne e1 e2 = BitVector.mk_ne e1 e2 `I32Type

    let lt e1 e2 = BitVector.mk_lt_s e1 e2 `I32Type

    let gt e1 e2 = BitVector.mk_gt_s e1 e2 `I32Type

    let lt_u e1 e2 = BitVector.mk_lt_u e1 e2 `I32Type

    let gt_u e1 e2 = BitVector.mk_gt_u e1 e2 `I32Type

    let le e1 e2 = BitVector.mk_le_s e1 e2 `I32Type

    let ge e1 e2 = BitVector.mk_ge_s e1 e2 `I32Type

    let le_u e1 e2 = BitVector.mk_le_u e1 e2 `I32Type

    let ge_u e1 e2 = BitVector.mk_ge_u e1 e2 `I32Type
  end

  module I64 = struct
    type num = Expr.t
    type vbool = Expr.t
    type const = Int64.t

    let zero = mk_i64 0L

    let clz e = BitVector.mk_clz e `I64Type

    let ctz _ = failwith "i64_ctz: TODO"

    let popcnt _ = failwith "i64_popcnt: TODO"

    let add e1 e2 = BitVector.mk_add e1 e2 `I64Type

    let sub e1 e2 = BitVector.mk_sub e1 e2 `I64Type

    let mul e1 e2 = BitVector.mk_mul e1 e2 `I64Type

    let div e1 e2 = BitVector.mk_div_s e1 e2 `I64Type

    let unsigned_div e1 e2 = BitVector.mk_div_u e1 e2 `I64Type

    let rem e1 e2 = BitVector.mk_rem_s e1 e2 `I64Type

    let unsigned_rem e1 e2 = BitVector.mk_rem_u e1 e2 `I64Type

    let logand e1 e2 = BitVector.mk_and e1 e2 `I64Type

    let logor e1 e2 = BitVector.mk_or e1 e2 `I64Type

    let logxor e1 e2 = BitVector.mk_xor e1 e2 `I64Type

    let shl e1 e2 = BitVector.mk_shl e1 e2 `I64Type

    let shr_s e1 e2 = BitVector.mk_shr_s e1 e2 `I64Type

    let shr_u e1 e2 = BitVector.mk_shr_u e1 e2 `I64Type

    let rotl e1 e2 = BitVector.mk_rotl e1 e2 `I64Type

    let rotr e1 e2 = BitVector.mk_rotr e1 e2 `I64Type

    let eq_const e c = BitVector.mk_eq e (mk_i64 c) `I64Type

    let eq e1 e2 = BitVector.mk_eq e1 e2 `I64Type

    let ne e1 e2 = BitVector.mk_ne e1 e2 `I64Type

    let lt e1 e2 = BitVector.mk_lt_s e1 e2 `I64Type

    let gt e1 e2 = BitVector.mk_gt_s e1 e2 `I64Type

    let lt_u e1 e2 = BitVector.mk_lt_u e1 e2 `I64Type

    let gt_u e1 e2 = BitVector.mk_gt_u e1 e2 `I64Type

    let le e1 e2 = BitVector.mk_le_s e1 e2 `I64Type

    let ge e1 e2 = BitVector.mk_ge_s e1 e2 `I64Type

    let le_u e1 e2 = BitVector.mk_le_u e1 e2 `I64Type

    let ge_u e1 e2 = BitVector.mk_ge_u e1 e2 `I64Type

    let of_int32 e = Expr.Cvtop (Types.I64 Types.I32.ExtendSI32, e)

    let to_int32 e = Expr.Cvtop (Types.I32 Types.I32.WrapI64 , e)
  end

  module F32 = struct
    type num = Expr.t
    type vbool = Expr.t

    let zero = mk_f32 0l

    let abs _ = assert false

    let neg _ = assert false

    let sqrt _ = assert false

    let ceil _ = assert false

    let floor _ = assert false

    let trunc _ = assert false

    let nearest _ = assert false

    let add _ = assert false

    let sub _ = assert false

    let mul _ = assert false

    let div _ = assert false

    let min _ = assert false

    let max _ = assert false

    let copy_sign _ = assert false

    let eq _ = assert false

    let ne _ = assert false

    let lt _ = assert false

    let gt _ = assert false

    let le _ = assert false

    let ge _ = assert false
  end

  module F64 = struct
    type num = Expr.t
    type vbool = Expr.t

    let zero = mk_f64 0L

    let abs _ = assert false

    let neg _ = assert false

    let sqrt _ = assert false

    let ceil _ = assert false

    let floor _ = assert false

    let trunc _ = assert false

    let nearest _ = assert false

    let add _ = assert false

    let sub _ = assert false

    let mul _ = assert false

    let div _ = assert false

    let min _ = assert false

    let max _ = assert false

    let copy_sign _ = assert false

    let eq _ = assert false

    let ne _ = assert false

    let lt _ = assert false

    let gt _ = assert false

    let le _ = assert false

    let ge _ = assert false
  end
end

module Test : Value_intf.T = Symbolic
