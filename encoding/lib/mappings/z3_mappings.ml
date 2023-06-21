open Core
open Types

exception Error of string

type expr = Z3.Expr.expr
type model = Z3.Model.model
type solver = Z3.Solver.solver
type status = Z3.Solver.status
type optimize = Z3.Optimize.optimize

let ctx =
  Z3.mk_context
    [ ("model", "true"); ("proof", "false"); ("unsat_core", "false") ]

let int_sort = Z3.Arithmetic.Integer.mk_sort ctx
let real_sort = Z3.Arithmetic.Real.mk_sort ctx
let bool_sort = Z3.Boolean.mk_sort ctx
let str_sort = Z3.Seq.mk_string_sort ctx
let bv32_sort = Z3.BitVector.mk_sort ctx 32
let bv64_sort = Z3.BitVector.mk_sort ctx 64
let fp32_sort = Z3.FloatingPoint.mk_sort_single ctx
let fp64_sort = Z3.FloatingPoint.mk_sort_double ctx
let rne = Z3.FloatingPoint.RoundingMode.mk_rne ctx
let rtz = Z3.FloatingPoint.RoundingMode.mk_rtz ctx

let get_sort (e : Types.expr_type) : Z3.Sort.sort =
  match e with
  | `IntType -> int_sort
  | `RealType -> real_sort
  | `BoolType -> bool_sort
  | `StrType -> str_sort
  | `I32Type -> bv32_sort
  | `I64Type -> bv64_sort
  | `F32Type -> fp32_sort
  | `F64Type -> fp64_sort

let encode_bool ~(to_bv : bool) (cond : Z3.Expr.expr) : Z3.Expr.expr =
  let bv_true = Z3.BitVector.mk_numeral ctx "1" 32
  and bv_false = Z3.BitVector.mk_numeral ctx "0" 32 in
  if to_bv then Z3.Boolean.mk_ite ctx cond bv_true bv_false else cond

module IntZ3Op = struct
  open I
  open Z3

  let int2str = FuncDecl.mk_func_decl_s ctx "IntToString" [ int_sort ] str_sort
  let str2int = FuncDecl.mk_func_decl_s ctx "StringToInt" [ str_sort ] int_sort
  let encode_num (i : Int.t) : Expr.expr = Expr.mk_numeral_int ctx i int_sort

  let encode_unop (op : unop) (e : Expr.expr) : Expr.expr =
    let op' = match op with Neg -> Arithmetic.mk_unary_minus ctx in
    op' e

  let encode_binop (op : binop) (e1 : Expr.expr) (e2 : Expr.expr) : Expr.expr =
    let op' =
      match op with
      | Add -> fun v1 v2 -> Arithmetic.mk_add ctx [ v1; v2 ]
      | Sub -> fun v1 v2 -> Arithmetic.mk_sub ctx [ v1; v2 ]
      | Mul -> fun v1 v2 -> Arithmetic.mk_mul ctx [ v1; v2 ]
      | Div -> Arithmetic.mk_div ctx
      | Rem -> Arithmetic.Integer.mk_rem ctx
      | Pow -> Arithmetic.mk_power ctx
      | _ -> raise (Error "Unsupported integer operations")
    in
    op' e1 e2

  let encode_relop (op : relop) (e1 : Expr.expr) (e2 : Expr.expr) : Expr.expr =
    let op' =
      match op with
      | Eq -> Boolean.mk_eq ctx
      | Ne -> fun v1 v2 -> Boolean.mk_eq ctx v1 v2 |> Boolean.mk_not ctx
      | Lt -> Arithmetic.mk_lt ctx
      | Gt -> Arithmetic.mk_gt ctx
      | Le -> Arithmetic.mk_le ctx
      | Ge -> Arithmetic.mk_ge ctx
    in
    op' e1 e2

  let encode_cvtop (op : cvtop) (e : Expr.expr) : Expr.expr =
    let op' =
      match op with
      | ToString -> fun v -> FuncDecl.apply int2str [ v ]
      | OfString -> fun v -> FuncDecl.apply str2int [ v ]
      | ReinterpretReal -> Arithmetic.Real.mk_real2int ctx
    in
    op' e

  let encode_triop (_ : triop) (_ : Expr.expr) (_ : Expr.expr) (_ : Expr.expr) :
      Expr.expr =
    assert false
end

module RealZ3Op = struct
  open R
  open Z3

  let real2str =
    FuncDecl.mk_func_decl_s ctx "RealToString" [ real_sort ] str_sort

  let str2real =
    FuncDecl.mk_func_decl_s ctx "StringToReal" [ str_sort ] real_sort

  let to_uint32 = FuncDecl.mk_func_decl_s ctx "ToUInt32" [ real_sort ] real_sort

  let encode_num (f : Float.t) : Expr.expr =
    Arithmetic.Real.mk_numeral_s ctx (Float.to_string f)

  let encode_unop (op : unop) (e : Expr.expr) : Expr.expr =
    let op' =
      match op with
      | Neg -> Arithmetic.mk_unary_minus ctx
      | Abs ->
          fun e ->
            Boolean.mk_ite ctx
              (Arithmetic.mk_gt ctx e (encode_num 0.))
              e
              (Arithmetic.mk_unary_minus ctx e)
      | Sqrt -> fun e -> Arithmetic.mk_power ctx e (encode_num 0.5)
      | Nearest | IsNan -> assert false
    in
    op' e

  let encode_binop (op : binop) (e1 : Expr.expr) (e2 : Expr.expr) : Expr.expr =
    let op' =
      match op with
      | Add -> fun v1 v2 -> Arithmetic.mk_add ctx [ v1; v2 ]
      | Sub -> fun v1 v2 -> Arithmetic.mk_sub ctx [ v1; v2 ]
      | Mul -> fun v1 v2 -> Arithmetic.mk_mul ctx [ v1; v2 ]
      | Div -> Arithmetic.mk_div ctx
      | Min ->
          fun v1 v2 -> Boolean.mk_ite ctx (Arithmetic.mk_le ctx v1 v2) v1 v2
      | Max ->
          fun v1 v2 -> Boolean.mk_ite ctx (Arithmetic.mk_ge ctx v1 v2) v1 v2
      | _ -> assert false
    in
    op' e1 e2

  let encode_relop (op : relop) (e1 : Expr.expr) (e2 : Expr.expr) : Expr.expr =
    let op' =
      match op with
      | Eq -> Boolean.mk_eq ctx
      | Ne -> fun v1 v2 -> Boolean.mk_eq ctx v1 v2 |> Boolean.mk_not ctx
      | Lt -> Arithmetic.mk_lt ctx
      | Gt -> Arithmetic.mk_gt ctx
      | Le -> Arithmetic.mk_le ctx
      | Ge -> Arithmetic.mk_ge ctx
    in
    op' e1 e2

  let encode_cvtop (op : cvtop) (e : Expr.expr) : Expr.expr =
    let op' =
      match op with
      | ToString -> fun v -> FuncDecl.apply real2str [ v ]
      | OfString -> fun v -> FuncDecl.apply str2real [ v ]
      | ConvertUI32 -> fun v -> FuncDecl.apply to_uint32 [ v ]
      | ReinterpretInt -> Arithmetic.Integer.mk_int2real ctx
      | DemoteF64 | ConvertSI32 | ConvertSI64 | ConvertUI64 | PromoteF32 ->
          assert false
    in
    op' e

  let encode_triop (_ : triop) (_ : Expr.expr) (_ : Expr.expr) (_ : Expr.expr) :
      Expr.expr =
    assert false
end

module BoolZ3Op = struct
  open B
  open Z3

  let encode_bool (b : Bool.t) : Expr.expr = Boolean.mk_val ctx b

  let encode_unop (op : unop) (e : Expr.expr) : Expr.expr =
    let op' = match op with Not -> Boolean.mk_not ctx in
    op' e

  let encode_binop (op : binop) (e1 : Expr.expr) (e2 : Expr.expr) : Expr.expr =
    let op' =
      match op with
      | And -> fun v1 v2 -> Boolean.mk_and ctx [ v1; v2 ]
      | Or -> fun v1 v2 -> Boolean.mk_or ctx [ v1; v2 ]
      | Xor -> Boolean.mk_xor ctx
    in
    op' e1 e2

  let encode_relop (op : relop) (e1 : Expr.expr) (e2 : Expr.expr) : Expr.expr =
    let op' =
      match op with
      | Eq -> Boolean.mk_eq ctx
      | Ne -> fun v1 v2 -> Boolean.mk_eq ctx v1 v2 |> Boolean.mk_not ctx
    in
    op' e1 e2

  let encode_cvtop (_ : cvtop) (_ : Expr.expr) : Expr.expr = assert false

  let encode_triop (op : triop) (e1 : Expr.expr) (e2 : Expr.expr)
      (e3 : Expr.expr) : Expr.expr =
    let op' = match op with ITE -> Boolean.mk_ite ctx in
    op' e1 e2 e3
end

module StrZ3Op = struct
  open S
  open Z3

  let encode_str (s : String.t) : Expr.expr = Seq.mk_string ctx s
  let trim = FuncDecl.mk_func_decl_s ctx "Trim" [ str_sort ] str_sort

  let encode_unop (op : unop) (e : Expr.expr) : Expr.expr =
    let op' =
      match op with
      | Len -> Seq.mk_seq_length ctx
      | Trim -> fun v -> FuncDecl.apply trim [ v ]
    in
    op' e

  let encode_binop (op : binop) (e1 : Expr.expr) (e2 : Expr.expr) : Expr.expr =
    let op' =
      match op with
      | Nth ->
          fun v1 v2 ->
            Seq.mk_seq_extract ctx v1 v2 (Expr.mk_numeral_int ctx 1 int_sort)
      | Concat -> fun v1 v2 -> Seq.mk_seq_concat ctx [ v1; v2 ]
    in
    op' e1 e2

  let encode_relop (op : relop) (e1 : Expr.expr) (e2 : Expr.expr) : Expr.expr =
    let op' =
      match op with
      | Eq -> Boolean.mk_eq ctx
      | Ne -> fun v1 v2 -> Boolean.mk_eq ctx v1 v2 |> Boolean.mk_not ctx
    in
    op' e1 e2

  let encode_triop (op : triop) (e1 : Expr.expr) (e2 : Expr.expr)
      (e3 : Expr.expr) : Expr.expr =
    let op' = match op with SubStr -> Seq.mk_seq_extract ctx in
    op' e1 e2 e3

  let encode_cvtop (_ : cvtop) (_ : Expr.expr) : Expr.expr = assert false
end

module I32Z3Op = struct
  open I32
  open Z3

  let encode_num (i : Int32.t) : Expr.expr =
    Expr.mk_numeral_int ctx (Int32.to_int_exn i) bv32_sort

  let encode_unop (op : unop) (e : Expr.expr) : Expr.expr =
    let op' =
      match op with
      | Not -> BitVector.mk_not ctx
      | Clz -> failwith "I32Z3Op: Clz not supported yet"
    in
    op' e

  let encode_binop (op : binop) (e1 : Expr.expr) (e2 : Expr.expr) : Expr.expr =
    let op' =
      match op with
      | Add -> BitVector.mk_add ctx
      | Sub -> BitVector.mk_sub ctx
      | Mul -> BitVector.mk_mul ctx
      | DivS -> BitVector.mk_sdiv ctx
      | DivU -> BitVector.mk_udiv ctx
      | And -> BitVector.mk_and ctx
      | Xor -> BitVector.mk_xor ctx
      | Or -> BitVector.mk_or ctx
      | Shl -> BitVector.mk_shl ctx
      | ShrS -> BitVector.mk_ashr ctx
      | ShrU -> BitVector.mk_lshr ctx
      | RemS -> BitVector.mk_srem ctx
      | RemU -> BitVector.mk_urem ctx
      | Rotl | Rotr -> failwith "z3_mappings: rotl|rotr not implemented!"
    in
    op' e1 e2

  let encode_relop ?(to_bv = false) (op : relop) (e1 : Expr.expr)
      (e2 : Expr.expr) : Expr.expr =
    let op' =
      match op with
      | Eq -> Boolean.mk_eq ctx
      | Ne -> fun x1 x2 -> Boolean.mk_eq ctx x1 x2 |> Boolean.mk_not ctx
      | LtU -> BitVector.mk_ult ctx
      | LtS -> BitVector.mk_slt ctx
      | LeU -> BitVector.mk_ule ctx
      | LeS -> BitVector.mk_sle ctx
      | GtU -> BitVector.mk_ugt ctx
      | GtS -> BitVector.mk_sgt ctx
      | GeU -> BitVector.mk_uge ctx
      | GeS -> BitVector.mk_sge ctx
    in
    encode_bool ~to_bv (op' e1 e2)

  let encode_cvtop (op : cvtop) (e : Expr.expr) : Expr.expr =
    let op' =
      match op with
      | WrapI64 -> BitVector.mk_extract ctx 31 0
      | TruncSF32 -> fun f -> FloatingPoint.mk_to_sbv ctx rtz f 32
      | TruncUF32 -> fun f -> FloatingPoint.mk_to_ubv ctx rtz f 32
      | TruncSF64 -> fun f -> FloatingPoint.mk_to_sbv ctx rtz f 32
      | TruncUF64 -> fun f -> FloatingPoint.mk_to_ubv ctx rtz f 32
      | ReinterpretFloat -> FloatingPoint.mk_to_ieee_bv ctx
      | ExtendSI32 | ExtendUI32 -> assert false
    in
    op' e

  let encode_triop (_ : triop) (_ : Expr.expr) (_ : Expr.expr) (_ : Expr.expr) :
      Expr.expr =
    assert false
end

module I64Z3Op = struct
  open I64
  open Z3

  let encode_num (i : Int64.t) : Expr.expr =
    Expr.mk_numeral_int ctx (Int64.to_int_exn i) bv64_sort

  let encode_unop (op : unop) (e : Expr.expr) : Expr.expr =
    let op' =
      match op with
      | Not -> BitVector.mk_not ctx
      | Clz -> failwith "I64Z3Op: clz supported yet"
    in
    op' e

  let encode_binop (op : binop) (e1 : Expr.expr) (e2 : Expr.expr) : Expr.expr =
    let op' =
      match op with
      | Add -> BitVector.mk_add ctx
      | Sub -> BitVector.mk_sub ctx
      | Mul -> BitVector.mk_mul ctx
      | DivS -> BitVector.mk_sdiv ctx
      | DivU -> BitVector.mk_udiv ctx
      | And -> BitVector.mk_and ctx
      | Xor -> BitVector.mk_xor ctx
      | Or -> BitVector.mk_or ctx
      | Shl -> BitVector.mk_shl ctx
      | ShrS -> BitVector.mk_ashr ctx
      | ShrU -> BitVector.mk_lshr ctx
      | RemS -> BitVector.mk_srem ctx
      | RemU -> BitVector.mk_urem ctx
      | Rotl | Rotr -> failwith "z3_mappings: rotl|rotr not implemented!"
    in
    op' e1 e2

  let encode_relop ?(to_bv = false) (op : relop) (e1 : Expr.expr)
      (e2 : Expr.expr) : Expr.expr =
    let op' =
      match op with
      | Eq -> Boolean.mk_eq ctx
      | Ne -> fun x1 x2 -> Boolean.mk_eq ctx x1 x2 |> Boolean.mk_not ctx
      | LtU -> BitVector.mk_ult ctx
      | LtS -> BitVector.mk_slt ctx
      | LeU -> BitVector.mk_ule ctx
      | LeS -> BitVector.mk_sle ctx
      | GtU -> BitVector.mk_ugt ctx
      | GtS -> BitVector.mk_sgt ctx
      | GeU -> BitVector.mk_uge ctx
      | GeS -> BitVector.mk_sge ctx
    in
    encode_bool ~to_bv (op' e1 e2)

  let encode_cvtop (op : cvtop) (e : Expr.expr) : Expr.expr =
    let op' =
      match op with
      | ExtendSI32 -> BitVector.mk_sign_ext ctx 32
      | ExtendUI32 -> BitVector.mk_zero_ext ctx 32
      (* rounding towards zero (aka truncation) *)
      | TruncSF32 -> fun f -> FloatingPoint.mk_to_sbv ctx rtz f 64
      | TruncUF32 -> fun f -> FloatingPoint.mk_to_ubv ctx rtz f 64
      | TruncSF64 -> fun f -> FloatingPoint.mk_to_sbv ctx rtz f 64
      | TruncUF64 -> fun f -> FloatingPoint.mk_to_ubv ctx rtz f 64
      | ReinterpretFloat -> FloatingPoint.mk_to_ieee_bv ctx
      | WrapI64 -> assert false
    in
    op' e

  let encode_triop (_ : triop) (_ : Expr.expr) (_ : Expr.expr) (_ : Expr.expr) :
      Expr.expr =
    assert false
end

module F32Z3Op = struct
  open F32
  open Z3

  let f322str = FuncDecl.mk_func_decl_s ctx "F32ToString" [ fp32_sort ] str_sort
  let str2f32 = FuncDecl.mk_func_decl_s ctx "StringToF32" [ str_sort ] fp32_sort

  let encode_num (f : Int32.t) : Expr.expr =
    FloatingPoint.mk_numeral_f ctx (Int32.float_of_bits f) fp32_sort

  let encode_unop (op : unop) (e : Expr.expr) : Expr.expr =
    let op' =
      match op with
      | Neg -> FloatingPoint.mk_neg ctx
      | Abs -> FloatingPoint.mk_abs ctx
      | Sqrt -> FloatingPoint.mk_sqrt ctx rne
      | Nearest -> FloatingPoint.mk_round_to_integral ctx rne
      | IsNan -> FloatingPoint.mk_is_nan ctx
    in
    op' e

  let encode_binop (op : binop) (e1 : Expr.expr) (e2 : Expr.expr) : Expr.expr =
    let op' =
      match op with
      | Add -> FloatingPoint.mk_add ctx rne
      | Sub -> FloatingPoint.mk_sub ctx rne
      | Mul -> FloatingPoint.mk_mul ctx rne
      | Div -> FloatingPoint.mk_div ctx rne
      | Min -> FloatingPoint.mk_min ctx
      | Max -> FloatingPoint.mk_max ctx
      | Rem -> FloatingPoint.mk_rem ctx
    in
    op' e1 e2

  let encode_relop ?(to_bv = false) (op : relop) (e1 : Expr.expr)
      (e2 : Expr.expr) : Expr.expr =
    let op' =
      match op with
      | Eq -> FloatingPoint.mk_eq ctx
      | Ne -> fun x1 x2 -> FloatingPoint.mk_eq ctx x1 x2 |> Boolean.mk_not ctx
      | Lt -> FloatingPoint.mk_lt ctx
      | Le -> FloatingPoint.mk_leq ctx
      | Gt -> FloatingPoint.mk_gt ctx
      | Ge -> FloatingPoint.mk_geq ctx
    in
    encode_bool ~to_bv (op' e1 e2)

  let encode_cvtop (op : cvtop) (e : Expr.expr) : Expr.expr =
    let op' =
      match op with
      | DemoteF64 -> fun bv -> FloatingPoint.mk_to_fp_float ctx rne bv fp32_sort
      | ConvertSI32 ->
          fun bv -> FloatingPoint.mk_to_fp_signed ctx rne bv fp32_sort
      | ConvertUI32 ->
          fun bv -> FloatingPoint.mk_to_fp_unsigned ctx rne bv fp32_sort
      | ConvertSI64 ->
          fun bv -> FloatingPoint.mk_to_fp_signed ctx rne bv fp32_sort
      | ConvertUI64 ->
          fun bv -> FloatingPoint.mk_to_fp_unsigned ctx rne bv fp32_sort
      | ReinterpretInt -> fun bv -> FloatingPoint.mk_to_fp_bv ctx bv fp32_sort
      | ToString -> fun v -> FuncDecl.apply f322str [ v ]
      | OfString -> fun v -> FuncDecl.apply str2f32 [ v ]
      | PromoteF32 -> assert false
    in
    op' e

  let encode_triop (_ : triop) (_ : Expr.expr) (_ : Expr.expr) (_ : Expr.expr) :
      Expr.expr =
    assert false
end

module F64Z3Op = struct
  open F64
  open Z3

  let f642str = FuncDecl.mk_func_decl_s ctx "F64ToString" [ fp64_sort ] str_sort
  let str2f64 = FuncDecl.mk_func_decl_s ctx "StringToF64" [ str_sort ] fp64_sort

  let encode_num (f : Int64.t) : Expr.expr =
    FloatingPoint.mk_numeral_f ctx (Int64.float_of_bits f) fp64_sort

  let encode_unop (op : unop) (e : Expr.expr) : Expr.expr =
    let op' =
      match op with
      | Neg -> FloatingPoint.mk_neg ctx
      | Abs -> FloatingPoint.mk_abs ctx
      | Sqrt -> FloatingPoint.mk_sqrt ctx rne
      | Nearest -> FloatingPoint.mk_round_to_integral ctx rne
      | IsNan -> FloatingPoint.mk_is_nan ctx
    in
    op' e

  let encode_binop (op : binop) (e1 : Expr.expr) (e2 : Expr.expr) : Expr.expr =
    let op' =
      match op with
      | Add -> FloatingPoint.mk_add ctx rne
      | Sub -> FloatingPoint.mk_sub ctx rne
      | Mul -> FloatingPoint.mk_mul ctx rne
      | Div -> FloatingPoint.mk_div ctx rne
      | Min -> FloatingPoint.mk_min ctx
      | Max -> FloatingPoint.mk_max ctx
      | Rem -> FloatingPoint.mk_rem ctx
    in
    op' e1 e2

  let encode_relop ?(to_bv = false) (op : relop) (e1 : Expr.expr)
      (e2 : Expr.expr) : Expr.expr =
    let op' =
      match op with
      | Eq -> FloatingPoint.mk_eq ctx
      | Ne -> fun x1 x2 -> FloatingPoint.mk_eq ctx x1 x2 |> Boolean.mk_not ctx
      | Lt -> FloatingPoint.mk_lt ctx
      | Le -> FloatingPoint.mk_leq ctx
      | Gt -> FloatingPoint.mk_gt ctx
      | Ge -> FloatingPoint.mk_geq ctx
    in
    encode_bool ~to_bv (op' e1 e2)

  let encode_cvtop (op : cvtop) (e : Expr.expr) : Expr.expr =
    let op' =
      match op with
      | PromoteF32 ->
          fun bv -> FloatingPoint.mk_to_fp_float ctx rne bv fp64_sort
      | ConvertSI32 ->
          fun bv -> FloatingPoint.mk_to_fp_signed ctx rne bv fp64_sort
      | ConvertUI32 ->
          fun bv -> FloatingPoint.mk_to_fp_unsigned ctx rne bv fp64_sort
      | ConvertSI64 ->
          fun bv -> FloatingPoint.mk_to_fp_signed ctx rne bv fp64_sort
      | ConvertUI64 ->
          fun bv -> FloatingPoint.mk_to_fp_unsigned ctx rne bv fp64_sort
      | ReinterpretInt -> fun bv -> FloatingPoint.mk_to_fp_bv ctx bv fp64_sort
      | ToString -> fun v -> FuncDecl.apply f642str [ v ]
      | OfString -> fun v -> FuncDecl.apply str2f64 [ v ]
      | DemoteF64 -> assert false
    in
    op' e

  let encode_triop (_ : triop) (_ : Expr.expr) (_ : Expr.expr) (_ : Expr.expr) :
      Expr.expr =
    assert false
end

let num i32 i64 f32 f64 : Num.t -> Z3.Expr.expr = function
  | I32 x -> i32 x
  | I64 x -> i64 x
  | F32 x -> f32 x
  | F64 x -> f64 x

let encode_num : Num.t -> Z3.Expr.expr =
  num I32Z3Op.encode_num I64Z3Op.encode_num F32Z3Op.encode_num
    F64Z3Op.encode_num

let encode_unop : unop -> Z3.Expr.expr -> Z3.Expr.expr =
  op IntZ3Op.encode_unop RealZ3Op.encode_unop BoolZ3Op.encode_unop
    StrZ3Op.encode_unop I32Z3Op.encode_unop I64Z3Op.encode_unop
    F32Z3Op.encode_unop F64Z3Op.encode_unop

let encode_binop : binop -> Z3.Expr.expr -> Z3.Expr.expr -> Z3.Expr.expr =
  op IntZ3Op.encode_binop RealZ3Op.encode_binop BoolZ3Op.encode_binop
    StrZ3Op.encode_binop I32Z3Op.encode_binop I64Z3Op.encode_binop
    F32Z3Op.encode_binop F64Z3Op.encode_binop

let encode_triop :
    triop -> Z3.Expr.expr -> Z3.Expr.expr -> Z3.Expr.expr -> Z3.Expr.expr =
  op IntZ3Op.encode_triop RealZ3Op.encode_triop BoolZ3Op.encode_triop
    StrZ3Op.encode_triop I32Z3Op.encode_triop I64Z3Op.encode_triop
    F32Z3Op.encode_triop F64Z3Op.encode_triop

let encode_relop ~to_bv : relop -> Z3.Expr.expr -> Z3.Expr.expr -> Z3.Expr.expr
    =
  op IntZ3Op.encode_relop RealZ3Op.encode_relop BoolZ3Op.encode_relop
    StrZ3Op.encode_relop
    (I32Z3Op.encode_relop ~to_bv)
    (I64Z3Op.encode_relop ~to_bv)
    (F32Z3Op.encode_relop ~to_bv)
    (F64Z3Op.encode_relop ~to_bv)

let encode_cvtop : cvtop -> Z3.Expr.expr -> Z3.Expr.expr =
  op IntZ3Op.encode_cvtop RealZ3Op.encode_cvtop BoolZ3Op.encode_cvtop
    StrZ3Op.encode_cvtop I32Z3Op.encode_cvtop I64Z3Op.encode_cvtop
    F32Z3Op.encode_cvtop F64Z3Op.encode_cvtop

let encode_quantifier (t : bool) (vars_list : Symbol.t list)
    (body : Z3.Expr.expr) (patterns : Z3.Quantifier.Pattern.pattern list) :
    Z3.Expr.expr =
  if List.length vars_list > 0 then
    let quantified_assertion =
      Z3.Quantifier.mk_quantifier_const ctx t
        (List.map vars_list ~f:(fun s ->
             Z3.Expr.mk_const_s ctx (Symbol.to_string s)
               (get_sort (Symbol.type_of s))))
        body None patterns [] None None
    in
    let quantified_assertion =
      Z3.Quantifier.expr_of_quantifier quantified_assertion
    in
    let quantified_assertion = Z3.Expr.simplify quantified_assertion None in
    quantified_assertion
  else body

let rec encode_expr ?(bool_to_bv = false) (e : Expression.t) : expr =
  let open Expression in
  match e with
  | Val (Int i) -> IntZ3Op.encode_num i
  | Val (Real r) -> RealZ3Op.encode_num r
  | Val (Bool b) -> BoolZ3Op.encode_bool b
  | Val (Num v) -> encode_num v
  | Val (Str s) -> StrZ3Op.encode_str s
  | SymPtr (base, offset) ->
      let base' = encode_num (I32 base) in
      let offset' = encode_expr offset in
      I32Z3Op.encode_binop I32.Add base' offset'
  | Unop (op, e) ->
      let e' = encode_expr e in
      encode_unop op e'
  | Binop ((Int _ as op), e1, e2) | Binop ((Bool _ as op), e1, e2) ->
      let e1' = encode_expr e1 and e2' = encode_expr e2 in
      encode_binop op e1' e2'
  | Binop (op, e1, e2) ->
      let e1' = encode_expr ~bool_to_bv:true e1
      and e2' = encode_expr ~bool_to_bv:true e2 in
      encode_binop op e1' e2'
  | Triop (op, e1, e2, e3) ->
      let e1' = encode_expr ~bool_to_bv e1
      and e2' = encode_expr ~bool_to_bv e2
      and e3' = encode_expr ~bool_to_bv e3 in
      encode_triop op e1' e2' e3'
  | Relop ((Int _ as op), e1, e2) | Relop ((Bool _ as op), e1, e2) ->
      let e1' = encode_expr e1 and e2' = encode_expr e2 in
      encode_relop ~to_bv:false op e1' e2'
  | Relop (op, e1, e2) ->
      let e1' = encode_expr ~bool_to_bv:true e1
      and e2' = encode_expr ~bool_to_bv:true e2 in
      encode_relop ~to_bv:bool_to_bv op e1' e2'
  | Cvtop (op, e) ->
      let e' = encode_expr e in
      encode_cvtop op e'
  | Symbol s ->
      let x = Symbol.to_string s and t = Symbol.type_of s in
      Z3.Expr.mk_const_s ctx x (get_sort t)
  | Extract (e, h, l) ->
      let e' = encode_expr ~bool_to_bv:true e in
      Z3.BitVector.mk_extract ctx ((h * 8) - 1) (l * 8) e'
  | Concat (e1, e2) ->
      let e1' = encode_expr e1 and e2' = encode_expr e2 in
      Z3.BitVector.mk_concat ctx e1' e2'
  | Quantifier (t, vars, body, patterns) ->
      let body' = encode_expr body in
      let encode_pattern p =
        Z3.Quantifier.mk_pattern ctx (List.map ~f:encode_expr p)
      in
      let patterns' = List.map ~f:encode_pattern patterns in
      let t' = match t with Forall -> true | Exists -> false in
      encode_quantifier t' vars body' patterns'

let expr_to_smtstring (es : Expression.t list) (status : bool) =
  let es' = List.map ~f:encode_expr es in
  Z3.Params.set_print_mode ctx Z3enums.PRINT_SMTLIB2_COMPLIANT;
  Z3.SMT.benchmark_to_smtstring ctx "" "" (Bool.to_string status) ""
    (List.tl_exn es') (List.hd_exn es')

let mk_solver () : solver = Z3.Solver.mk_solver ctx None
let interrupt () = Z3.Tactic.interrupt ctx
let translate (s : solver) : solver = Z3.Solver.translate s ctx
let add_solver (s : solver) (es : expr list) : unit = Z3.Solver.add s es
let check (s : solver) (es : expr list) : status = Z3.Solver.check s es
let get_model (s : solver) : model option = Z3.Solver.get_model s
let mk_opt () : optimize = Z3.Optimize.mk_opt ctx
let add_opt (o : optimize) (es : expr list) : unit = Z3.Optimize.add o es

let maximize (o : optimize) (e : expr) : Z3.Optimize.handle =
  Z3.Optimize.maximize o e

let minimize (o : optimize) (e : expr) : Z3.Optimize.handle =
  Z3.Optimize.minimize o e

let get_opt_model (o : optimize) : model Option.t = Z3.Optimize.get_model o

let set (s : string) (i : int) (n : char) =
  let bs = Bytes.of_string s in
  Bytes.set bs i n;
  Bytes.to_string bs

let int64_of_bv (bv : Z3.Expr.expr) : int64 =
  assert (Z3.Expr.is_numeral bv);
  Int64.of_string (Z3.BitVector.numeral_to_string bv)

let int64_of_fp (fp : Z3.Expr.expr) ~(ebits : int) ~(sbits : int) : int64 =
  assert (Z3.Expr.is_numeral fp);
  if Z3.FloatingPoint.is_numeral_nan ctx fp then
    if Z3.FloatingPoint.is_numeral_negative ctx fp then
      if sbits = 23 then Int64.of_int32 0xffc0_0000l else 0xfff8_0000_0000_0000L
    else if sbits = 23 then Int64.of_int32 0x7fc0_0000l
    else 0x7ff8_0000_0000_0000L
  else if Z3.FloatingPoint.is_numeral_inf ctx fp then
    if Z3.FloatingPoint.is_numeral_negative ctx fp then
      if sbits = 23 then Int64.of_int32 (Int32.bits_of_float (-.(1.0 /. 0.0)))
      else Int64.bits_of_float (-.(1.0 /. 0.0))
    else if sbits = 23 then Int64.of_int32 (Int32.bits_of_float (1.0 /. 0.0))
    else Int64.bits_of_float (1.0 /. 0.0)
  else if Z3.FloatingPoint.is_numeral_zero ctx fp then
    if Z3.FloatingPoint.is_numeral_negative ctx fp then
      if sbits = 23 then Int64.of_int32 0x8000_0000l else 0x8000_0000_0000_0000L
    else if sbits = 23 then Int64.of_int32 (Int32.bits_of_float 0.0)
    else Int64.bits_of_float 0.0
  else
    let fp = Z3.Expr.to_string fp in
    let fp = Caml.String.sub fp 4 (String.length fp - 5) in
    let fp_list =
      List.map ~f:(fun fp -> set fp 0 '0') (String.split ~on:' ' fp)
    in
    let bit_list = List.map ~f:(fun fp -> Int64.of_string fp) fp_list in
    let fp_sign = Int64.shift_left (List.nth_exn bit_list 0) (ebits + sbits)
    and exponent = Int64.shift_left (List.nth_exn bit_list 1) sbits
    and fraction = List.nth_exn bit_list 2 in
    Int64.(fp_sign lor (exponent lor fraction))

let value_of_const (model : Z3.Model.model) (c : Expression.t) : Value.t option
    =
  let t = Expression.type_of c
  and interp = Z3.Model.eval model (encode_expr c) true in
  let f (e : Z3.Expr.expr) : Value.t =
    match (t, Z3.Sort.get_sort_kind (Z3.Expr.get_sort e)) with
    | `IntType, Z3enums.INT_SORT ->
        Int (Int.of_string (Z3.Arithmetic.Integer.numeral_to_string e))
    | `RealType, Z3enums.REAL_SORT ->
        Real (Float.of_string (Z3.Arithmetic.Real.to_decimal_string e 6))
    | `BoolType, Z3enums.BOOL_SORT ->
        Bool (Bool.of_string (Z3.Expr.to_string e))
    | `StrType, Z3enums.SEQ_SORT -> Str (Z3.Seq.get_string ctx e)
    | `I32Type, Z3enums.BV_SORT ->
        Num (I32 (Int64.to_int32_trunc (int64_of_bv e)))
    | `I64Type, Z3enums.BV_SORT -> Num (I64 (int64_of_bv e))
    | `F32Type, Z3enums.FLOATING_POINT_SORT ->
        let ebits = Z3.FloatingPoint.get_ebits ctx (Z3.Expr.get_sort e)
        and sbits = Z3.FloatingPoint.get_sbits ctx (Z3.Expr.get_sort e) - 1 in
        Num (F32 (Int64.to_int32_trunc (int64_of_fp e ~ebits ~sbits)))
    | `F64Type, Z3enums.FLOATING_POINT_SORT ->
        let ebits = Z3.FloatingPoint.get_ebits ctx (Z3.Expr.get_sort e)
        and sbits = Z3.FloatingPoint.get_sbits ctx (Z3.Expr.get_sort e) - 1 in
        Num (F64 (int64_of_fp e ~ebits ~sbits))
    | _ -> assert false
  in
  Option.map ~f interp

let type_of_sort (sort : Z3.Sort.sort) : expr_type =
  match Z3.Sort.get_sort_kind sort with
  | Z3enums.INT_SORT -> `IntType
  | Z3enums.REAL_SORT -> `RealType
  | Z3enums.BOOL_SORT -> `BoolType
  | Z3enums.SEQ_SORT -> `StrType
  | Z3enums.BV_SORT ->
      if Z3.BitVector.get_size sort = 32 then `I32Type else `I64Type
  | Z3enums.FLOATING_POINT_SORT ->
      if Z3.FloatingPoint.get_sbits ctx sort = 23 then `F32Type else `F64Type
  | _ -> assert false

let symbols_of_model (model : Z3.Model.model) : Symbol.t list =
  List.map (Z3.Model.get_const_decls model) ~f:(fun const ->
      let x = Z3.Symbol.to_string (Z3.FuncDecl.get_name const) in
      let t = type_of_sort (Z3.FuncDecl.get_range const) in
      Symbol.mk_symbol t x)

let model_binds (model : Z3.Model.model) (symbols : Symbol.t list) :
    (Symbol.t * Value.t) list =
  List.fold_left symbols ~init:[] ~f:(fun a s ->
      let v = value_of_const model (Expression.mk_symbol s) in
      Option.fold ~init:a ~f:(fun a v' -> (s, v') :: a) v)

let value_binds ?(symbols : Symbol.t list option) (model : Z3.Model.model) :
    (Symbol.t * Value.t) list =
  let symbols' = Option.value symbols ~default:(symbols_of_model model) in
  model_binds model symbols'

let string_binds (m : Z3.Model.model) : (string * string * string) list =
  List.map (Z3.Model.get_const_decls m) ~f:(fun const ->
      let sort = Z3.Sort.to_string (Z3.FuncDecl.get_range const)
      and name = Z3.Symbol.to_string (Z3.FuncDecl.get_name const)
      and interp =
        Option.value_map ~default:"" ~f:Z3.Expr.to_string
          (Z3.Model.get_const_interp m const)
      in
      (sort, name, interp))
