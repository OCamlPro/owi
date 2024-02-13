(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021 Léo Andrès *)
(* Copyright © 2021 Pierre Chambart *)

open Encoding
open Ty
open Expr
open Hc

let ( let+ ) o f = Option.map f o

let unop ty op e =
  match e.node.e with
  | Val (Num n) -> Val (Num (Eval_numeric.eval_unop ty op n)) @: e.node.ty
  | _ -> Unop (op, e) @: ty

let binop ty op e1 e2 =
  match (e1.node.e, e2.node.e) with
  | Val (Num n1), Val (Num n2) ->
    Val (Num (Eval_numeric.eval_binop ty op n1 n2)) @: ty
  | Ptr _, _ | _, Ptr _ ->
    (* Does pointer arithmetic *)
    Expr.simplify @@ Binop (op, e1, e2) @: Ty_bitv S32
  | _ -> Binop (op, e1, e2) @: ty

let relop ty op e1 e2 =
  match (e1.node.e, e2.node.e) with
  | Val (Num n1), Val (Num n2) ->
    Val (if Eval_numeric.eval_relop ty op n1 n2 then True else False) @: ty
  | Val (Num n), Ptr (b, { node = { e = Val (Num o); _ }; _ }) ->
    let base = Eval_numeric.eval_binop (Ty_bitv S32) Add (I32 b) o in
    Val (if Eval_numeric.eval_relop ty op n base then True else False) @: ty
  | Ptr (b, { node = { e = Val (Num o); _ }; _ }), Val (Num n) ->
    let base = Eval_numeric.eval_binop (Ty_bitv S32) Add (I32 b) o in
    let b = Eval_numeric.eval_relop ty op base n in
    Val (if b then True else False) @: ty
  | _ -> Relop (op, e1, e2) @: ty

let cvtop ty op e =
  match e.node.e with
  | Val (Num n) -> Val (Num (Eval_numeric.eval_cvtop ty op n)) @: ty
  | _ -> Cvtop (op, e) @: ty

module S = struct
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

  let const_i32 (i : Int32.t) : int32 = Val (Num (I32 i)) @: Ty_bitv S32

  let const_i64 (i : Int64.t) : int64 = Val (Num (I64 i)) @: Ty_bitv S64

  let const_f32 (f : Float32.t) : float32 =
    Val (Num (F32 (Float32.to_bits f))) @: Ty_fp S32

  let const_f64 (f : Float64.t) : float64 =
    Val (Num (F64 (Float64.to_bits f))) @: Ty_fp S64

  let ref_null _ty = Ref (Funcref None)

  let ref_func f : t = Ref (Funcref (Some f))

  let ref_externref t v : t = Ref (Externref (Some (E (t, v))))

  let ref_is_null = function
    | Funcref (Some _) | Externref (Some _) -> Val False @: Ty_bool
    | Funcref None | Externref None -> Val True @: Ty_bool

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
    let of_val = function
      | Val True -> Some true
      | Val False -> Some false
      | _ -> None

    let const b = Val (if b then True else False) @: Ty_bool

    let not e =
      match e.node.e with
      | Unop (Not, cond) -> cond
      | e' ->
        Option.value ~default:(Unop (Not, e) @: Ty_bool)
        @@ let+ b = of_val e' in
           const (not b)

    let or_ e1 e2 =
      match (of_val e1.node.e, of_val e2.node.e) with
      | Some b1, Some b2 -> const (b1 || b2)
      | Some false, _ -> e2
      | _, Some false -> e1
      | Some true, _ | _, Some true -> const true
      | _ -> Binop (Or, e1, e2) @: Ty_bool

    let and_ e1 e2 =
      match (of_val e1.node.e, of_val e2.node.e) with
      | Some b1, Some b2 -> const (b1 && b2)
      | Some true, _ -> e2
      | _, Some true -> e1
      | Some false, _ | _, Some false -> const false
      | _ -> Binop (And, e1, e2) @: Ty_bool

    let int32 e =
      match e.node.e with
      | Val True -> const_i32 1l
      | Val False -> const_i32 0l
      | Cvtop (ToBool, ({ node = { ty = Ty_bitv S32; _ }; _ } as e')) -> e'
      | _ -> Cvtop (OfBool, e) @: Ty_bitv S32

    let select_expr c ~if_true ~if_false =
      match of_val c.node.e with
      | Some true -> if_true
      | Some false -> if_false
      | None -> Triop (Ite, c, if_true, if_false) @: Ty_bool

    let pp ppf (e : vbool) = Expr.pp ppf e
  end

  module I32 = struct
    type num = Expr.t

    type vbool = Expr.t

    type const = Int64.t

    type nonrec float32 = float32

    type nonrec float64 = float64

    let ty = Ty_bitv S32

    let zero = const_i32 0l

    let clz e = unop ty Clz e

    let ctz _ = failwith "i32_ctz: TODO"

    let popcnt _ = failwith "i32_popcnt: TODO"

    let add e1 e2 = binop ty Add e1 e2

    let sub e1 e2 = binop ty Sub e1 e2

    let mul e1 e2 = binop ty Mul e1 e2

    let div e1 e2 = binop ty Div e1 e2

    let unsigned_div e1 e2 = binop ty DivU e1 e2

    let rem e1 e2 = binop ty Rem e1 e2

    let unsigned_rem e1 e2 = binop ty RemU e1 e2

    let boolify e =
      match e.node.e with
      | Val (Num (I32 0l)) -> Some (Bool.const false)
      | Val (Num (I32 1l)) -> Some (Bool.const true)
      | Cvtop (OfBool, cond) -> Some cond
      | _ -> None

    let logand e1 e2 =
      match (boolify e1, boolify e2) with
      | Some b1, Some b2 -> Bool.int32 (Bool.and_ b1 b2)
      | _ -> binop ty And e1 e2

    let logor e1 e2 =
      match (boolify e1, boolify e2) with
      | Some b1, Some b2 -> Bool.int32 (Bool.or_ b1 b2)
      | _ -> binop ty Or e1 e2

    let logxor e1 e2 = binop ty Xor e1 e2

    let shl e1 e2 = binop ty Shl e1 e2

    let shr_s e1 e2 = binop ty ShrA e1 e2

    let shr_u e1 e2 = binop ty ShrL e1 e2

    let rotl e1 e2 = binop ty Rotl e1 e2

    let rotr e1 e2 = binop ty Rotr e1 e2

    let eq_const e c =
      match e.node.e with
      | Cvtop (OfBool, cond) -> begin
        match c with 0l -> Bool.not cond | 1l -> cond | _ -> Bool.const false
      end
      | _ -> relop ty Eq e (const_i32 c)

    let eq e1 e2 = if e1 == e2 then Bool.const true else relop ty Eq e1 e2

    let ne e1 e2 = if e1 == e2 then Bool.const false else relop ty Ne e1 e2

    let lt e1 e2 = if e1 == e2 then Bool.const false else relop ty Lt e1 e2

    let gt e1 e2 = if e1 == e2 then Bool.const false else relop ty Gt e1 e2

    let lt_u e1 e2 = if e1 == e2 then Bool.const false else relop ty LtU e1 e2

    let gt_u e1 e2 = if e1 == e2 then Bool.const false else relop ty GtU e1 e2

    let le e1 e2 = if e1 == e2 then Bool.const true else relop ty Le e1 e2

    let ge e1 e2 = if e1 == e2 then Bool.const true else relop ty Ge e1 e2

    let le_u e1 e2 = if e1 == e2 then Bool.const true else relop ty LeU e1 e2

    let ge_u e1 e2 = if e1 == e2 then Bool.const true else relop ty GeU e1 e2

    let to_bool (e : vbool) =
      match e.node.e with
      | Val (Num (I32 i)) -> Bool.const @@ Int32.ne i 0l
      | Cvtop (OfBool, cond) -> cond
      | _ -> Cvtop (ToBool, e) @: ty

    let trunc_f32_s x = cvtop ty TruncSF32 x

    let trunc_f32_u x = cvtop ty TruncUF32 x

    let trunc_f64_s x = cvtop ty TruncSF64 x

    let trunc_f64_u x = cvtop ty TruncUF64 x

    let trunc_sat_f32_s _ = assert false

    let trunc_sat_f32_u _ = assert false

    let trunc_sat_f64_s _ = assert false

    let trunc_sat_f64_u _ = assert false

    let reinterpret_f32 x = cvtop ty Reinterpret_float x

    let wrap_i64 x = cvtop ty WrapI64 x

    (* FIXME: This is probably wrong? *)
    let extend_s n x =
      cvtop ty (ExtS (32 - n)) (Extract (x, n / 8, 0) @: Ty_bitv S32)
  end

  module I64 = struct
    type num = Expr.t

    type vbool = Expr.t

    type const = Int64.t

    type nonrec float32 = float32

    type nonrec float64 = float64

    let ty = Ty_bitv S64

    let zero = const_i64 0L

    let clz e = unop ty Clz e

    let ctz _ = failwith "i64_ctz: TODO"

    let popcnt _ = failwith "i64_popcnt: TODO"

    let add e1 e2 = binop ty Add e1 e2

    let sub e1 e2 = binop ty Sub e1 e2

    let mul e1 e2 = binop ty Mul e1 e2

    let div e1 e2 = binop ty Div e1 e2

    let unsigned_div e1 e2 = binop ty DivU e1 e2

    let rem e1 e2 = binop ty Rem e1 e2

    let unsigned_rem e1 e2 = binop ty RemU e1 e2

    let logand e1 e2 = binop ty And e1 e2

    let logor e1 e2 = binop ty Or e1 e2

    let logxor e1 e2 = binop ty Xor e1 e2

    let shl e1 e2 = binop ty Shl e1 e2

    let shr_s e1 e2 = binop ty ShrA e1 e2

    let shr_u e1 e2 = binop ty ShrL e1 e2

    let rotl e1 e2 = binop ty Rotl e1 e2

    let rotr e1 e2 = binop ty Rotr e1 e2

    let eq_const e c = relop ty Eq e (const_i64 c)

    let eq e1 e2 = relop ty Eq e1 e2

    let ne e1 e2 = relop ty Ne e1 e2

    let lt e1 e2 = relop ty Lt e1 e2

    let gt e1 e2 = relop ty Gt e1 e2

    let lt_u e1 e2 = relop ty LtU e1 e2

    let gt_u e1 e2 = relop ty GtU e1 e2

    let le e1 e2 = relop ty Le e1 e2

    let ge e1 e2 = relop ty Ge e1 e2

    let le_u e1 e2 = relop ty LeU e1 e2

    let ge_u e1 e2 = relop ty GeU e1 e2

    let of_int32 e = cvtop ty (ExtS 32) e

    let to_int32 e = cvtop (Ty_bitv S32) WrapI64 e

    let trunc_f32_s x = cvtop ty TruncSF32 x

    let trunc_f32_u x = cvtop ty TruncUF32 x

    let trunc_f64_s x = cvtop ty TruncSF64 x

    let trunc_f64_u x = cvtop ty TruncUF64 x

    let trunc_sat_f32_s _ = assert false

    let trunc_sat_f32_u _ = assert false

    let trunc_sat_f64_s _ = assert false

    let trunc_sat_f64_u _ = assert false

    let reinterpret_f64 x = cvtop ty Reinterpret_float x

    (* FIXME: This is probably wrong? *)
    let extend_s n x =
      cvtop ty (ExtS (64 - n)) (Extract (x, n / 8, 0) @: Ty_bitv S64)

    let extend_i32_s x = cvtop ty (ExtS 32) x

    let extend_i32_u x = cvtop ty (ExtU 32) x
  end

  module F32 = struct
    type num = Expr.t

    type vbool = Expr.t

    type nonrec int32 = int32

    type nonrec int64 = int64

    type same_size_int = int32

    let ty = Ty_fp S32

    let zero = const_f32 Float32.zero

    let abs x = unop ty Abs x

    let neg x = unop ty Neg x

    let sqrt x = unop ty Sqrt x

    let ceil x = unop ty Ceil x

    let floor x = unop ty Floor x

    let trunc x = unop ty Trunc x

    let nearest x = unop ty Nearest x

    let add x y = binop ty Add x y

    let sub x y = binop ty Sub x y

    let mul x y = binop ty Mul x y

    let div x y = binop ty Div x y

    let min x y = binop ty Min x y

    let max x y = binop ty Max x y

    let copy_sign _ _ = assert false

    let eq x y = relop ty Eq x y

    let ne x y = relop ty Ne x y

    let lt x y = relop ty Lt x y

    let gt x y = relop ty Gt x y

    let le x y = relop ty Le x y

    let ge x y = relop ty Ge x y

    let convert_i32_s x = cvtop ty ConvertSI32 x

    let convert_i32_u x = cvtop ty ConvertUI32 x

    let convert_i64_s x = cvtop ty ConvertSI64 x

    let convert_i64_u x = cvtop ty ConvertUI64 x

    let demote_f64 x = cvtop ty DemoteF64 x

    let reinterpret_i32 x = cvtop ty Reinterpret_int x

    let of_bits x = cvtop ty Reinterpret_int x

    let to_bits x = cvtop (Ty_bitv S32) Reinterpret_float x
  end

  module F64 = struct
    type num = Expr.t

    type vbool = Expr.t

    type nonrec int32 = int32

    type nonrec int64 = int64

    type same_size_int = int64

    let ty = Ty_fp S64

    let zero = const_f64 Float64.zero

    let abs x = unop ty Abs x

    let neg x = unop ty Neg x

    let sqrt x = unop ty Sqrt x

    let ceil x = unop ty Ceil x

    let floor x = unop ty Floor x

    let trunc x = unop ty Trunc x

    let nearest x = unop ty Nearest x

    let add x y = binop ty Add x y

    let sub x y = binop ty Sub x y

    let mul x y = binop ty Mul x y

    let div x y = binop ty Div x y

    let min x y = binop ty Min x y

    let max x y = binop ty Max x y

    let copy_sign _ _ = assert false

    let eq x y = relop ty Eq x y

    let ne x y = relop ty Ne x y

    let lt x y = relop ty Lt x y

    let gt x y = relop ty Gt x y

    let le x y = relop ty Le x y

    let ge x y = relop ty Ge x y

    let convert_i32_s x = cvtop ty ConvertSI32 x

    let convert_i32_u x = cvtop ty ConvertUI32 x

    let convert_i64_s x = cvtop ty ConvertSI64 x

    let convert_i64_u x = cvtop ty ConvertUI64 x

    let promote_f32 x = cvtop ty PromoteF32 x

    let reinterpret_i64 x = cvtop ty Reinterpret_int x

    let of_bits x = cvtop ty Reinterpret_int x

    let to_bits x = cvtop (Ty_bitv S64) Reinterpret_float x
  end
end

module S' : Value_intf.T = S
