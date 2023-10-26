(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021 Léo Andrès *)
(* Copyright © 2021 Pierre Chambart *)

open Encoding
open Expression

let ( let* ) o f = Option.bind o f

let ( let+ ) o f = Option.map f o

let return = Option.some

let mk_i32 x = Val (Num (I32 x))

let mk_i64 x = Val (Num (I64 x))

let mk_f32 x = Val (Num (F32 x))

let mk_f64 x = Val (Num (F64 x))

let unop op e =
  match e with
  | Val (Num n) -> Val (Num (Eval_numeric.eval_unop op n))
  | _ -> Unop (op, e)

let binop op e1 e2 =
  match (e1, e2) with
  | Val (Num n1), Val (Num n2) -> Val (Num (Eval_numeric.eval_binop op n1 n2))
  | Val (Bool _), Val (Bool _) -> assert false
  | _ -> Binop (op, e1, e2)

let relop op e1 e2 =
  match (e1, e2) with
  | Val (Num n1), Val (Num n2) -> Val (Bool (Eval_numeric.eval_relop op n1 n2))
  | _ -> Relop (op, e1, e2)

let cvtop op e =
  match e with
  | Val (Num n) -> Val (Num (Eval_numeric.eval_cvtop op n))
  | _ -> Cvtop (op, e)

module S = struct
  module Expr = Encoding.Expression

  type vbool = Expr.t

  type int32 = Expr.t

  type int64 = Expr.t

  type float32 = Expr.t

  type float64 = Expr.t

  type externref = Concrete_value.externref

  type ref_value =
    | Funcref of Func_intf.t option
    | Externref of externref option

  type t =
    | I32 of int32
    | I64 of int64
    | F32 of float32
    | F64 of float64
    | Ref of ref_value

  let const_i32 (i : Int32.t) : int32 = mk_i32 i

  let const_i64 (i : Int64.t) : int64 = mk_i64 i

  let const_f32 (f : Float32.t) : float32 = mk_f32 (Float32.to_bits f)

  let const_f64 (f : Float64.t) : float64 = mk_f64 (Float64.to_bits f)

  let ref_null _ty = Ref (Funcref None)

  let ref_func f : t = Ref (Funcref (Some f))

  let ref_externref t v : t = Ref (Externref (Some (E (t, v))))

  let ref_is_null = function
    | Funcref (Some _) | Externref (Some _) -> Val (Bool false)
    | Funcref None | Externref None -> Val (Bool true)

  let pp ppf v =
    let e =
      match v with
      | I32 e -> e
      | I64 e -> e
      | F32 e -> e
      | F64 e -> e
      | Ref _ -> assert false
    in
    Expr.pp ppf e

  module Ref = struct
    let get_func (r : ref_value) : Func_intf.t Value_intf.get_ref =
      match r with
      | Funcref (Some f) -> Ref_value f
      | Funcref None -> Null
      | Externref _ -> Type_mismatch

    let get_externref (type t) (r : ref_value) (t : t Type.Id.t) :
      t Value_intf.get_ref =
      match r with
      | Externref (Some (E (ety, v))) -> (
        match Type.Id.provably_equal t ety with
        | None -> assert false
        | Some Equal -> Ref_value v )
      | _ -> assert false
  end

  module Bool = struct
    let of_val = function Val (Bool b) -> Some b | _ -> None

    let const b = Val (Bool b)

    let not e =
      match e with
      | Unop (Bool Not, cond) -> cond
      | _ ->
        Option.value ~default:(Boolean.mk_not e)
        @@ let+ b = of_val e in
           Val (Bool (not b))

    let or_ e1 e2 =
      match (of_val e1, of_val e2) with
      | Some b1, Some b2 -> Val (Bool (b1 || b2))
      | Some false, _ -> e2
      | _, Some false -> e1
      | Some true, _ | _, Some true -> Val (Bool true)
      | _ -> Boolean.mk_or e1 e2

    let and_ e1 e2 =
      match (of_val e1, of_val e2) with
      | Some b1, Some b2 -> Val (Bool (b1 && b2))
      | Some true, _ -> e2
      | _, Some true -> e1
      | Some false, _ | _, Some false -> Val (Bool false)
      | _ -> Boolean.mk_and e1 e2

    let int32 = function
      | Val (Bool b) -> if b then mk_i32 1l else mk_i32 0l
      | Cvtop (I32 ToBool, e) -> e
      | e -> Cvtop (I32 OfBool, e)

    let select_expr c ~if_true ~if_false =
      match of_val c with
      | Some true -> if_true
      | Some false -> if_false
      | None -> Boolean.mk_ite c if_true if_false

    let pp ppf (e : vbool) = Format.pp_print_string ppf (Expr.to_string e)
  end

  module I32 = struct
    open Expr

    type num = Expr.t

    type vbool = Expr.t

    type const = Int64.t

    type nonrec float32 = float32

    type nonrec float64 = float64

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

    let boolify e =
      match e with
      | Val (Num (I32 0l)) -> Some (Val (Bool false))
      | Val (Num (I32 1l)) -> Some (Val (Bool true))
      | Cvtop (I32 OfBool, cond) -> Some cond
      | _ -> None

    let logand e1 e2 =
      match (boolify e1, boolify e2) with
      | Some b1, Some b2 -> Bool.int32 (Bool.and_ b1 b2)
      | _ -> binop (I32 And) e1 e2

    let logor e1 e2 =
      match (boolify e1, boolify e2) with
      | Some b1, Some b2 -> Bool.int32 (Bool.or_ b1 b2)
      | _ -> binop (I32 Or) e1 e2

    let logxor e1 e2 = binop (I32 Xor) e1 e2

    let shl e1 e2 = binop (I32 Shl) e1 e2

    let shr_s e1 e2 = binop (I32 ShrS) e1 e2

    let shr_u e1 e2 = binop (I32 ShrU) e1 e2

    let rotl e1 e2 = binop (I32 Rotl) e1 e2

    let rotr e1 e2 = binop (I32 Rotr) e1 e2

    let eq_const e c =
      match e with
      | Cvtop (I32 OfBool, cond) -> begin
        match c with 0l -> Bool.not cond | 1l -> cond | _ -> Val (Bool false)
      end
      | _ -> relop (I32 Eq) e (Val (Num (I32 c)))

    let eq e1 e2 = if e1 == e2 then Val (Bool true) else relop (I32 Eq) e1 e2

    let ne e1 e2 = if e1 == e2 then Val (Bool false) else relop (I32 Ne) e1 e2

    let lt e1 e2 = if e1 == e2 then Val (Bool false) else relop (I32 LtS) e1 e2

    let gt e1 e2 = if e1 == e2 then Val (Bool false) else relop (I32 GtS) e1 e2

    let lt_u e1 e2 =
      if e1 == e2 then Val (Bool false) else relop (I32 LtU) e1 e2

    let gt_u e1 e2 =
      if e1 == e2 then Val (Bool false) else relop (I32 GtU) e1 e2

    let le e1 e2 = if e1 == e2 then Val (Bool true) else relop (I32 LeS) e1 e2

    let ge e1 e2 = if e1 == e2 then Val (Bool true) else relop (I32 GeS) e1 e2

    let le_u e1 e2 = if e1 == e2 then Val (Bool true) else relop (I32 LeU) e1 e2

    let ge_u e1 e2 = if e1 == e2 then Val (Bool true) else relop (I32 GeU) e1 e2

    let to_bool (e : vbool) =
      match e with
      | Val (Num (I32 i)) -> Val (Bool (Int32.ne i 0l))
      | Cvtop (I32 OfBool, cond) -> cond
      | e -> Cvtop (I32 ToBool, e)

    let trunc_f32_s x = cvtop (I32 TruncSF32) x

    let trunc_f32_u x = cvtop (I32 TruncUF32) x

    let trunc_f64_s x = cvtop (I32 TruncSF64) x

    let trunc_f64_u x = cvtop (I32 TruncUF64) x

    let trunc_sat_f32_s _ = assert false

    let trunc_sat_f32_u _ = assert false

    let trunc_sat_f64_s _ = assert false

    let trunc_sat_f64_u _ = assert false

    let reinterpret_f32 x = cvtop (I32 ReinterpretFloat) x

    let wrap_i64 x = cvtop (I32 WrapI64) x

    let extend_s _ = assert false
  end

  module I64 = struct
    open Expr

    type num = Expr.t

    type vbool = Expr.t

    type const = Int64.t

    type nonrec float32 = float32

    type nonrec float64 = float64

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

    let of_int32 e = cvtop (I64 ExtendSI32) e

    let to_int32 e = cvtop (I32 WrapI64) e

    let trunc_f32_s x = cvtop (I64 TruncSF32) x

    let trunc_f32_u x = cvtop (I64 TruncUF32) x

    let trunc_f64_s x = cvtop (I64 TruncSF64) x

    let trunc_f64_u x = cvtop (I64 TruncUF64) x

    let trunc_sat_f32_s _ = assert false

    let trunc_sat_f32_u _ = assert false

    let trunc_sat_f64_s _ = assert false

    let trunc_sat_f64_u _ = assert false

    let reinterpret_f64 x = cvtop (I64 ReinterpretFloat) x

    let extend_s _ = assert false

    let extend_i32_s x = cvtop (I64 ExtendSI32) x

    let extend_i32_u x = cvtop (I64 ExtendUI32) x
  end

  module F32 = struct
    type num = Expr.t

    type vbool = Expr.t

    type nonrec int32 = int32

    type nonrec int64 = int64

    type same_size_int = int32

    let zero = mk_f32 0l

    let abs x = unop (F32 Abs) x

    let neg x = unop (F32 Neg) x

    let sqrt x = unop (F32 Sqrt) x

    let ceil x = unop (F32 Ceil) x

    let floor x = unop (F32 Floor) x

    let trunc _ = assert false

    let nearest x = unop (F32 Nearest) x

    let add x y = binop (F32 Add) x y

    let sub x y = binop (F32 Sub) x y

    let mul x y = binop (F32 Mul) x y

    let div x y = binop (F32 Div) x y

    let min x y = binop (F32 Min) x y

    let max x y = binop (F32 Max) x y

    let copy_sign _ _ = assert false

    let eq x y = relop (F32 Eq) x y

    let ne x y = relop (F32 Ne) x y

    let lt x y = relop (F32 Lt) x y

    let gt x y = relop (F32 Gt) x y

    let le x y = relop (F32 Le) x y

    let ge x y = relop (F32 Ge) x y

    let convert_i32_s x = cvtop (F32 ConvertSI32) x

    let convert_i32_u x = cvtop (F32 ConvertUI32) x

    let convert_i64_s x = cvtop (F32 ConvertSI64) x

    let convert_i64_u x = cvtop (F32 ConvertUI64) x

    let demote_f64 x = cvtop (F32 DemoteF64) x

    let reinterpret_i32 x = cvtop (F32 ReinterpretInt) x

    let of_bits x = cvtop (F32 ReinterpretInt) x

    let to_bits x = cvtop (I32 ReinterpretFloat) x
  end

  module F64 = struct
    type num = Expr.t

    type vbool = Expr.t

    type nonrec int32 = int32

    type nonrec int64 = int64

    type same_size_int = int64

    let zero = mk_f64 0L

    let abs x = unop (F64 Abs) x

    let neg x = unop (F64 Neg) x

    let sqrt x = unop (F64 Sqrt) x

    let ceil x = unop (F64 Ceil) x

    let floor x = unop (F64 Floor) x

    let trunc _x = assert false

    let nearest x = unop (F64 Nearest) x

    let add x y = binop (F64 Add) x y

    let sub x y = binop (F64 Sub) x y

    let mul x y = binop (F64 Mul) x y

    let div x y = binop (F64 Div) x y

    let min x y = binop (F64 Min) x y

    let max x y = binop (F64 Max) x y

    let copy_sign _ _ = assert false

    let eq x y = relop (F64 Eq) x y

    let ne x y = relop (F64 Ne) x y

    let lt x y = relop (F64 Lt) x y

    let gt x y = relop (F64 Gt) x y

    let le x y = relop (F64 Le) x y

    let ge x y = relop (F64 Ge) x y

    let convert_i32_s x = cvtop (F64 ConvertSI32) x

    let convert_i32_u x = cvtop (F64 ConvertUI32) x

    let convert_i64_s x = cvtop (F64 ConvertSI64) x

    let convert_i64_u x = cvtop (F64 ConvertUI64) x

    let promote_f32 x = cvtop (F64 PromoteF32) x

    let reinterpret_i64 x = cvtop (F64 ReinterpretInt) x

    let of_bits x = cvtop (F64 ReinterpretInt) x

    let to_bits x = cvtop (I64 ReinterpretFloat) x
  end
end

module S' : Value_intf.T = S
