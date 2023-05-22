open Crowbarplus
open Owi.Types
open Owi.Types.Symbolic
open Syntax

let num_type = choose [ const I32; const I64 ]

let sx = choose [ const U; const S ]

let val_type =
  let+ nt = [ num_type ] in
  Num_type nt

let mut = choose [ const Const; const Var ]

let div =
  let+ s = [ sx ] in
  (Div s : ibinop)

let rem =
  let+ s = [ sx ] in
  (Rem s : ibinop)

let shr =
  let+ s = [ sx ] in
  (Shr s : ibinop)

let ibinop =
  choose [
    const (Add : ibinop); const (Sub : ibinop); const (Mul : ibinop); div; rem;
    const (And : ibinop); const (Or : ibinop); const (Xor : ibinop);
    const (Shl : ibinop); shr;
    const (Rotl : ibinop); const (Rotr : ibinop)
  ]

let iunop = choose [ const Clz; const Ctz; const Popcnt]

let itestop = const Eqz

let ilt =
  let+ s = [ sx ] in
  (Lt s : irelop)

let igt =
  let+ s = [ sx ] in
  (Gt s : irelop)

let ile =
  let+ s = [ sx ] in
  (Le s : irelop)

let ige =
  let+ s = [ sx ] in
  (Ge s : irelop)

let irelop =
  choose [ const (Eq : irelop); const (Ne : irelop); ilt; igt; ile; ige ]

let const_i32 =
  let+ i = [ int32 ] in
  I32_const i

let const_i64 =
  let+ i = [ int64 ] in
  I64_const i

let ibinop_32 =
  let+ bop = [ ibinop ] in
  I_binop (S32, bop)

let ibinop_64 =
  let+ bop = [ ibinop ] in
  I_binop (S64, bop)

let iunop_32 =
  let+ uop = [ iunop ] in
  I_unop (S32, uop)

let iunop_64 =
  let+ uop = [ iunop ] in
  I_unop (S64, uop)

let itestop_32 =
  let+ top = [ itestop ] in
  I_testop (S32, top)

let itestop_64 =
  let+ top = [ itestop ] in
  I_testop (S64, top)

let irelop_32 =
  let+ rop = [ irelop ] in
  I_relop (S32, rop)

let irelop_64 =
  let+ rop = [ irelop ] in
  I_relop (S64, rop)

let extend_i32 =
  let+ s = [ sx ] in
  I64_extend_i32 s

let extend_32_i32 =
  choose [ const (I_extend8_s S32); const (I_extend16_s S32) ]

let extend_64_i64 =
  choose [ const (I_extend8_s S64); const (I_extend16_s S64); const I64_extend32_s ]

let funop = choose [
    const Abs; const Neg; const Sqrt;
    const Ceil; const Floor; const Trunc; const Nearest
  ]

let fbinop = choose [
  const Add; const Sub; const Mul;
  const Div; const Min; const Max; const Copysign
]

let frelop = choose [
  const Eq; const Ne; const Lt;
  const Gt; const Le; const Ge
]

let fbinop_32 =
  let+ bop = [ fbinop ] in
  F_binop (S32, bop)

let fbinop_64 =
  let+ bop = [ fbinop ] in
  F_binop (S64, bop)

let funop_32 =
  let+ uop = [ funop ] in
  F_unop (S32, uop)

let funop_64 =
  let+ uop = [ funop ] in
  F_unop (S64, uop)

let frelop_32 =
  let+ rop = [ frelop ] in
  F_relop (S32, rop)

let frelop_64 =
  let+ rop = [ frelop ] in
  F_relop (S64, rop)

(* let const_f64 =
  let+ f = [ float ] in
  F64_const (Owi.Float64.of_float f) *)

let f32_convert_i32 =
  let+ s = [ sx ] in
  F_convert_i (S32, S32, s)

let f32_convert_i64 =
  let+ s = [ sx ] in
  F_convert_i (S32, S64, s)

let f64_convert_i32 =
  let+ s = [ sx ] in
  F_convert_i (S64, S32, s)

let f64_convert_i64 =
  let+ s = [ sx ] in
  F_convert_i (S64, S64, s)

let const_of_num_type = function
  | I32 -> const_i32
  | I64 -> const_i64
  | _ ->
    (* TODO: complete *)
    assert false

let const_of_val_type = function
  | Num_type nt -> const_of_num_type nt
  | _ ->
    (* TODO: complete *)
    assert false

let global_type = pair mut val_type

let param =
  let* typ = val_type in
  let name = Env.add_local typ in
  const (Some name, typ)

let block_type =
  let param_type = list param in
  let result_type = list val_type in
  let typ = pair param_type result_type in
  let+ typ = [ typ ] in
  Arg.Bt_raw (None, typ)
