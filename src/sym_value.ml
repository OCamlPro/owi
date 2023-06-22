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

  let pp ppf v =
    let e = match v with
    | I32 e -> e
    | I64 e -> e
    | F32 e -> e
    | F64 e -> e
    | Ref _ -> assert false
    in
    Format.fprintf ppf "%s" (Encoding.Expression.to_string e)


  module Bool = struct
    let not = Boolean.mk_not

    let or_ = Boolean.mk_or

    let and_ = Boolean.mk_and

    let int32 = function
      | Expr.Val (Bool b) -> if b then mk_i32 0l else mk_i32 1l
      | e -> Boolean.mk_ite e (mk_i32 0l) (mk_i32 1l)
  end

  module I32 = struct
    open Expr

    type num = Expr.t

    type vbool = Expr.t

    type const = Int64.t

    type nonrec float32 = float32

    type nonrec float64 = float64

    let unop op e =
      match e with
      | Val (Num i) -> Val (Num (Eval_numeric.eval_unop op i))
      | e' -> Unop (op, e')

    let binop op e1 e2 =
      match (e1, e2) with
      | Val (Num i1), Val (Num i2) ->
        Val (Num (Eval_numeric.eval_binop op i1 i2))
      | Val (Bool _), Val (Bool _) -> assert false
      | e1', e2' -> Binop (op, e1', e2')

    let relop op e1 e2 =
      match (e1, e2) with
      | Val (Num i1), Val (Num i2) ->
        Val (Bool (Eval_numeric.eval_relop op i1 i2))
      | e1', e2' -> Relop (op, e1', e2')

    let zero = mk_i32 0l

    let clz e = unop (I32 Clz) e

    let ctz _ = failwith "i32_ctz: TODO"

    let popcnt _ = failwith "i32_popcnt: TODO"

    let add e1 e2 = binop (I32 Add) e1 e2

    let sub e1 e2 = binop (I32 Sub) e1 e2

    let mul e1 e2 = binop (I32 Mul) e1 e2

    let div e1 e2 = binop (I32 DivS) e1 e2

    let unsigned_div e1 e2 = binop (I32 DivU) e1 e2

    let rem e1 e2 = binop (I32 RemS) e1 e2

    let unsigned_rem e1 e2 = binop (I32 RemU) e1 e2

    let logand e1 e2 = binop (I32 And) e1 e2

    let logor e1 e2 = binop (I32 Or) e1 e2

    let logxor e1 e2 = binop (I32 Xor) e1 e2

    let shl e1 e2 = binop (I32 Shl) e1 e2

    let shr_s e1 e2 = binop (I32 ShrS) e1 e2

    let shr_u e1 e2 = binop (I32 ShrU) e1 e2

    let rotl e1 e2 = binop (I32 Rotl) e1 e2

    let rotr e1 e2 = binop (I32 Rotr) e1 e2

    let eq_const e c = relop (I32 Eq) e (Val (Num (I32 c)))

    let eq e1 e2 = relop (I32 Eq) e1 e2

    let ne e1 e2 = relop (I32 Ne) e1 e2

    let lt e1 e2 = relop (I32 LtS) e1 e2

    let gt e1 e2 = relop (I32 GtS) e1 e2

    let lt_u e1 e2 = relop (I32 LtU) e1 e2

    let gt_u e1 e2 = relop (I32 GtU) e1 e2

    let le e1 e2 = relop (I32 LeS) e1 e2

    let ge e1 e2 = relop (I32 GeS) e1 e2

    let le_u e1 e2 = relop (I32 LeU) e1 e2

    let ge_u e1 e2 = relop (I32 GeU) e1 e2

    let to_bool e = relop (I32 Ne) e (mk_i32 0l)

    let trunc_f32_s _ = assert false

    let trunc_f32_u _ = assert false

    let trunc_f64_s _ = assert false

    let trunc_f64_u _ = assert false

    let trunc_sat_f32_s _ = assert false

    let trunc_sat_f32_u _ = assert false

    let trunc_sat_f64_s _ = assert false

    let trunc_sat_f64_u _ = assert false

    let reinterpret_f32 _ = assert false

    let wrap_i64 _ = assert false

    let extend_s _ = assert false
  end

  module I64 = struct
    open Expr

    type num = Expr.t

    type vbool = Expr.t

    type const = Int64.t

    type nonrec float32 = float32

    type nonrec float64 = float64

    let unop op e =
      match e with
      | Val (Num i) -> Val (Num (Eval_numeric.eval_unop op i))
      | e' -> Unop (op, e')

    let binop op e1 e2 =
      match (e1, e2) with
      | Val (Num i1), Val (Num i2) ->
        Val (Num (Eval_numeric.eval_binop op i1 i2))
      | Val (Bool _), Val (Bool _) -> assert false
      | e1', e2' -> Binop (op, e1', e2')

    let relop op e1 e2 =
      match (e1, e2) with
      | Val (Num i1), Val (Num i2) ->
        Val (Bool (Eval_numeric.eval_relop op i1 i2))
      | e1', e2' -> Relop (op, e1', e2')

    let zero = mk_i64 0L

    let clz e = unop (I64 Clz) e

    let ctz _ = failwith "i64_ctz: TODO"

    let popcnt _ = failwith "i64_popcnt: TODO"

    let add e1 e2 = binop (I64 Add) e1 e2

    let sub e1 e2 = binop (I64 Sub) e1 e2

    let mul e1 e2 = binop (I64 Mul) e1 e2

    let div e1 e2 = binop (I64 DivS) e1 e2

    let unsigned_div e1 e2 = binop (I64 DivU) e1 e2

    let rem e1 e2 = binop (I64 RemS) e1 e2

    let unsigned_rem e1 e2 = binop (I64 RemU) e1 e2

    let logand e1 e2 = binop (I64 And) e1 e2

    let logor e1 e2 = binop (I64 Or) e1 e2

    let logxor e1 e2 = binop (I64 Xor) e1 e2

    let shl e1 e2 = binop (I64 Shl) e1 e2

    let shr_s e1 e2 = binop (I64 ShrS) e1 e2

    let shr_u e1 e2 = binop (I64 ShrU) e1 e2

    let rotl e1 e2 = binop (I64 Rotl) e1 e2

    let rotr e1 e2 = binop (I64 Rotr) e1 e2

    let eq_const e c = relop (I64 Eq) e (Val (Num (I64 c)))

    let eq e1 e2 = relop (I64 Eq) e1 e2

    let ne e1 e2 = relop (I64 Ne) e1 e2

    let lt e1 e2 = relop (I64 LtS) e1 e2

    let gt e1 e2 = relop (I64 GtS) e1 e2

    let lt_u e1 e2 = relop (I64 LtU) e1 e2

    let gt_u e1 e2 = relop (I64 GtU) e1 e2

    let le e1 e2 = relop (I64 LeS) e1 e2

    let ge e1 e2 = relop (I64 GeS) e1 e2

    let le_u e1 e2 = relop (I64 LeU) e1 e2

    let ge_u e1 e2 = relop (I64 GeU) e1 e2

    let of_int32 e = Expr.Cvtop (Types.I64 Types.I32.ExtendSI32, e)

    let to_int32 e = Expr.Cvtop (Types.I32 Types.I32.WrapI64, e)

    let trunc_f32_s _ = assert false

    let trunc_f32_u _ = assert false

    let trunc_f64_s _ = assert false

    let trunc_f64_u _ = assert false

    let trunc_sat_f32_s _ = assert false

    let trunc_sat_f32_u _ = assert false

    let trunc_sat_f64_s _ = assert false

    let trunc_sat_f64_u _ = assert false

    let reinterpret_f64 _ = assert false

    let extend_s _ = assert false

    let extend_i32_s _ = assert false

    let extend_i32_u _ = assert false
  end

  module F32 = struct
    type num = Expr.t

    type vbool = Expr.t

    type nonrec int32 = int32

    type nonrec int64 = int64

    let zero = mk_f32 0l

    let abs x = FloatingPoint.mk_abs x `F32Type

    let neg x = FloatingPoint.mk_neg x `F32Type

    let sqrt x = FloatingPoint.mk_sqrt x `F32Type

    let ceil _ = assert false

    let floor _ = assert false

    let trunc _ = assert false

    let nearest x = FloatingPoint.mk_nearest x `F32Type

    let add x y = FloatingPoint.mk_add x y `F32Type

    let sub x y = FloatingPoint.mk_sub x y `F32Type

    let mul x y = FloatingPoint.mk_mul x y `F32Type

    let div x y = FloatingPoint.mk_div x y `F32Type

    let min x y = FloatingPoint.mk_min x y `F32Type

    let max x y = FloatingPoint.mk_max x y `F32Type

    let copy_sign _ _ = assert false

    let eq x y = FloatingPoint.mk_eq x y `F32Type

    let ne x y = FloatingPoint.mk_ne x y `F32Type

    let lt x y = FloatingPoint.mk_lt x y `F32Type

    let gt x y = FloatingPoint.mk_gt x y `F32Type

    let le x y = FloatingPoint.mk_le x y `F32Type

    let ge x y = FloatingPoint.mk_ge x y `F32Type

    let convert_i32_s _ = assert false

    let convert_i32_u _ = assert false

    let convert_i64_s _ = assert false

    let convert_i64_u _ = assert false

    let demote_f64 _ = assert false

    let reinterpret_i32 _ = assert false
  end

  module F64 = struct
    type num = Expr.t

    type vbool = Expr.t

    type nonrec int32 = int32

    type nonrec int64 = int64

    let zero = mk_f64 0L

    let abs x = FloatingPoint.mk_abs x `F64Type

    let neg x = FloatingPoint.mk_neg x `F64Type

    let sqrt x = FloatingPoint.mk_sqrt x `F64Type

    let ceil _ = assert false

    let floor _ = assert false

    let trunc _ = assert false

    let nearest x = FloatingPoint.mk_nearest x `F64Type

    let add x y = FloatingPoint.mk_add x y `F64Type

    let sub x y = FloatingPoint.mk_sub x y `F64Type

    let mul x y = FloatingPoint.mk_mul x y `F64Type

    let div x y = FloatingPoint.mk_div x y `F64Type

    let min x y = FloatingPoint.mk_min x y `F64Type

    let max x y = FloatingPoint.mk_max x y `F64Type

    let copy_sign _ _ = assert false

    let eq x y = FloatingPoint.mk_eq x y `F64Type

    let ne x y = FloatingPoint.mk_ne x y `F64Type

    let lt x y = FloatingPoint.mk_lt x y `F64Type

    let gt x y = FloatingPoint.mk_gt x y `F64Type

    let le x y = FloatingPoint.mk_le x y `F64Type

    let ge x y = FloatingPoint.mk_ge x y `F64Type

    let convert_i32_s _ = assert false

    let convert_i32_u _ = assert false

    let convert_i64_s _ = assert false

    let convert_i64_u _ = assert false

    let promote_f32 _ = assert false

    let reinterpret_i64 _ = assert false
  end
end

module Test : Value_intf.T = Symbolic
