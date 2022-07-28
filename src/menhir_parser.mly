%token <String.t> NUM
%token <String.t> ID NAME
%token ALIGN ASSERT_EXHAUSTION ASSERT_INVALID ASSERT_MALFORMED ASSERT_RETURN ASSERT_TRAP ASSERT_UNLINKABLE BINARY BLOCK BR BR_IF BR_TABLE CALL CALL_INDIRECT DATA DATA_DROP DECLARE DROP ELEM ELEM_DROP ELSE END EOF EQUAL EXPORT EXTERN EXTERNREF F32 F32_ABS F32_ADD F32_CEIL F32_CONST F32_CONVERT_I32_S F32_CONVERT_I32_U F32_CONVERT_I64_S F32_CONVERT_I64_U F32_COPYSIGN F32_DEMOTE_F64 F32_DIV F32_EQ F32_FLOOR F32_GE F32_GT F32_LE F32_LOAD F32_LT F32_MAX F32_MIN F32_MUL F32_NE F32_NEAREST F32_NEG F32_REINTERPRET_I32 F32_REINTERPRET_I64 F32_SQRT F32_STORE F32_SUB F32_TRUNC F64 F64_ABS F64_ADD F64_CEIL F64_CONST F64_CONVERT_I32_S F64_CONVERT_I32_U F64_CONVERT_I64_S F64_CONVERT_I64_U F64_COPYSIGN F64_DIV F64_EQ F64_FLOOR F64_GE F64_GT F64_LE F64_LOAD F64_LT F64_MAX F64_MIN F64_MUL F64_NE F64_NEAREST F64_NEG F64_PROMOTE_F32 F64_REINTERPRET_I32 F64_REINTERPRET_I64 F64_SQRT F64_STORE F64_SUB F64_TRUNC FUNC FUNCREF GET GLOBAL GLOBAL_GET GLOBAL_SET I32 I32_ADD I32_AND I32_CLZ I32_CONST I32_CTZ I32_DIV_S I32_DIV_U I32_EQ I32_EQZ I32_EXTEND16_S I32_EXTEND8_S I32_GE_S I32_GE_U I32_GT_S I32_GT_U I32_LE_S I32_LE_U I32_LOAD I32_LOAD16_S I32_LOAD16_U I32_LOAD8_S I32_LOAD8_U I32_LT_S I32_LT_U I32_MUL I32_NE I32_OR I32_POPCNT I32_REINTERPRET_F32 I32_REINTERPRET_F64 I32_REM_S I32_REM_U I32_ROTL I32_ROTR I32_SHL I32_SHR_S I32_SHR_U I32_STORE I32_STORE16 I32_STORE8 I32_SUB I32_TRUNC_F32_S I32_TRUNC_F32_U I32_TRUNC_F64_S I32_TRUNC_F64_U I32_TRUNC_SAT_F32_S I32_TRUNC_SAT_F32_U I32_TRUNC_SAT_F64_S I32_TRUNC_SAT_F64_U I32_WRAP_I64 I32_XOR I64 I64_ADD I64_AND I64_CLZ I64_CONST I64_CTZ I64_DIV_S I64_DIV_U I64_EQ I64_EQZ I64_EXTEND16_S I64_EXTEND32_S I64_EXTEND8_S I64_EXTEND_I32_S I64_EXTEND_I32_U I64_GE_S I64_GE_U I64_GT_S I64_GT_U I64_LE_S I64_LE_U I64_LOAD I64_LOAD16_S I64_LOAD16_U I64_LOAD32_S I64_LOAD32_U I64_LOAD8_S I64_LOAD8_U I64_LT_S I64_LT_U I64_MUL I64_NE I64_OR I64_POPCNT I64_REINTERPRET_F32 I64_REINTERPRET_F64 I64_REM_S I64_REM_U I64_ROTL I64_ROTR I64_SHL I64_SHR_S I64_SHR_U I64_STORE I64_STORE16 I64_STORE32 I64_STORE8 I64_SUB I64_TRUNC_F32_S I64_TRUNC_F32_U I64_TRUNC_F64_S I64_TRUNC_F64_U I64_TRUNC_SAT_F32_S I64_TRUNC_SAT_F32_U I64_TRUNC_SAT_F64_S I64_TRUNC_SAT_F64_U I64_XOR IF IMPORT INVOKE ITEM LOCAL LOCAL_GET LOCAL_SET LOCAL_TEE LOOP LPAR MEMORY MEMORY_COPY MEMORY_FILL MEMORY_GROW MEMORY_INIT MEMORY_SIZE MODULE MUTABLE NAN_ARITH NAN_CANON NOP OFFSET PARAM QUOTE REF_EXTERN REF_FUNC REF_IS_NULL REF_NULL REGISTER RESULT RETURN RPAR SELECT START TABLE TABLE_COPY TABLE_FILL TABLE_GET TABLE_GROW TABLE_INIT TABLE_SET TABLE_SIZE THEN TYPE UNREACHABLE

%{

let u32 s =
  try Unsigned.UInt32.to_int (Unsigned.UInt32.of_string s)
  with Failure _msg -> failwith "constant out of range"

let i32 s =
  try Int32.of_string s
  with Failure _msg -> failwith "constant out of range"

let i64 s =
  try Int64.of_string s
  with Failure _msg -> failwith "constant out of range"

let f64 s =
  try Float64.of_string s
  with Failure _msg -> failwith "constant out of range"

let f32 s =
  try Float32.of_string s
  with Failure _msg -> failwith "constant out of range"

open Types

module Woi = struct end
%}

%start <Types.file> file

%%

(* Helpers *)

let utf8_name ==
  | name = NAME; {
    Wutf8.check_utf8 name;
    name
  }

let par(X) ==
  | LPAR; ~ = X; RPAR; <>

let string_list ==
  | l = list(NAME); { String.concat "" l }

(* Types *)

let ref_kind ==
  | FUNC; { Func_ref }
  | EXTERN; { Extern_ref }

let ref_type ==
  | FUNCREF; { Func_ref }
  | EXTERNREF; { Extern_ref }

let val_type :=
  | ~ = num_type; <Num_type>
  | ~ = ref_type; <Ref_type>

let global_type ==
  | ~ = val_type; { Const, val_type }
  | val_type = par(preceded(MUTABLE, val_type)); { Var, val_type }

let def_type ==
  | ~ = par(preceded(FUNC, func_type)); <>

let func_type :=
  | o = list(par(preceded(RESULT, list(val_type)))); { [], List.flatten o }
  | i = par(preceded(PARAM, list(val_type))); (i2, o) = func_type; {
    (List.map (fun i -> None, i) i) @ i2, o
  }
  | LPAR; PARAM; ~ = id; ~ = val_type; RPAR; (i2, o) = func_type; {
    (Some id, val_type)::i2, o
  }

let table_type ==
  | ~ = limits; ~ = ref_type; { limits, ref_type }

let mem_type ==
  | ~ = limits; <>

let limits ==
  | min = NUM; {
    let min = u32 min in
    let max = None in
    { min; max}
  }
  | min = NUM; max = NUM; {
    let min = u32 min in
    let max = Some (u32 max) in
    { min; max }
  }

let type_use ==
  | ~ = par(preceded(TYPE, indice)); <>

(* Immediates *)

(*
let num ==
  | ~ = NUM; <>
*)

(* var *)
let indice ==
  | ~ = ID; <Symbolic>
  | n = NUM; { Raw (u32 n) }

(* bind_var *)
let id ==
  | ~ = ID; <>


(* TODO: TO CLASSIFY *)

let num_type ==
  | I32; { Types.I32 }
  | I64; { Types.I64 }
  | F32; { Types.F32 }
  | F64; { Types.F64 }

let align ==
  | ALIGN; EQUAL; n = NUM; {
    let n = u32 n in
    if n = 0 || (Int.logand n (n - 1) <> 0) then failwith "alignment"
    else n
  }

let memarg_offset ==
  | OFFSET; EQUAL; ~ = NUM; <>

let memarg ==
  | offset = option(memarg_offset); align = option(align); {
    (* TODO: check default *)
    let offset = u32 @@ Option.value offset ~default:"0" in
    let align = Option.value align ~default:0 in
    {offset; align}
  }

let instr ==
| ~ = plain_instr; { [plain_instr] }
| (e, es) = select_instr_instr; { e::es }
| (e, es) = call_instr_instr; { e::es }
| ~ = block_instr; { [ block_instr ] }
| ~ = expr; { expr }

let plain_instr :=
  | NOP; { Nop }
  | UNREACHABLE; { Unreachable }
  | DROP; { Drop }
  | BR; ~ = indice; <Br>
  | BR_IF; ~ = indice; <Br_if>
  | BR_TABLE; l = nonempty_list(indice); {
    let xs = Array.of_list l in
    let n = Array.length xs in
    Br_table (Array.sub xs 0 (n - 1), xs.(n - 1))
  }
  | RETURN; { Return }
  | CALL; ~ = indice; <Call>
  | LOCAL_GET; ~ = indice; <Local_get>
  | LOCAL_SET; ~ = indice; <Local_set>
  | LOCAL_TEE; ~ = indice; <Local_tee>
  | GLOBAL_GET; ~ = indice; <Global_get>
  | GLOBAL_SET; ~ = indice; <Global_set>
  | TABLE_GET; indice = option(indice); {
    Table_get (Option.value indice ~default:(Raw 0))
  }
  | TABLE_SET; indice = option(indice); {
    Table_set (Option.value indice ~default:(Raw 0))
  }
  | TABLE_SIZE; indice = option(indice); {
    Table_size (Option.value indice ~default:(Raw 0))
  }
  | TABLE_GROW; indice = option(indice); {
    Table_grow (Option.value indice ~default:(Raw 0))
  }
  | TABLE_FILL; indice = option(indice); {
    Table_fill (Option.value indice ~default:(Raw 0))
  }
  | TABLE_COPY; {
    Table_copy (Raw 0, Raw 0)
  }
  | TABLE_COPY; src = indice; dst = indice; { Table_copy (src, dst) }
  | TABLE_INIT; t_indice = ioption(indice); ~ = indice; {
    Table_init (Option.value t_indice ~default:(Raw 0), indice)
  }
  (* TODO: check they're actually plain_instr and not instr: *)
  (* TODO: check that nothing is missing *)
  | ELEM_DROP; ~ = indice; <Elem_drop>
  | I32_CONST; n = NUM; { I32_const (i32 n) }
  | I64_CONST; n = NUM; { I64_const (i64 n) }
  | F32_CONST; n = NUM; { F32_const (f32 n) }
  | F64_CONST; n = NUM; { F64_const (f64 n) }
  | I32_CLZ; { I_unop (S32, Clz) }
  | I64_CLZ; { I_unop (S64, Clz) }
  | I32_CTZ; { I_unop (S32, Ctz) }
  | I64_CTZ; { I_unop (S64, Ctz) }
  | I32_POPCNT; { I_unop (S32, Popcnt) }
  | I64_POPCNT; { I_unop (S64, Popcnt) }
  | F32_ABS; { F_unop (S32, Abs) } 
  | F64_ABS; { F_unop (S64, Abs) }
  | F32_NEG; { F_unop (S32, Neg) } 
  | F64_NEG; { F_unop (S64, Neg) }
  | F32_SQRT; { F_unop (S32, Sqrt) } 
  | F64_SQRT; { F_unop (S64, Sqrt) }
  | F32_CEIL; { F_unop (S32, Ceil) }
  | F64_CEIL; { F_unop (S64, Ceil) }
  | F32_FLOOR; { F_unop (S32, Floor) } 
  | F64_FLOOR; { F_unop (S64, Floor) }
  | F32_TRUNC; { F_unop (S32, Trunc) }
  | F64_TRUNC; { F_unop (S64, Trunc) }
  | F32_NEAREST; { F_unop (S32, Nearest) }
  | F64_NEAREST; { F_unop (S64, Nearest) }
  | I32_ADD; { I_binop (S32, Add) }
  | I64_ADD; { I_binop (S64, Add) }
  | I32_SUB; { I_binop (S32, Sub) }
  | I64_SUB; { I_binop (S64, Sub) }
  | I32_MUL; { I_binop (S32, Mul) }
  | I64_MUL; { I_binop (S64, Mul) }
  | I32_DIV_S; { I_binop (S32, Div S) }
  | I64_DIV_S; { I_binop (S64, Div S) }
  | I32_DIV_U; { I_binop (S32, Div U) }
  | I64_DIV_U; { I_binop (S64, Div U) }
  | I32_REM_S; { I_binop (S32, Rem S) }
  | I64_REM_S; { I_binop (S64, Rem S) }
  | I32_REM_U; { I_binop (S32, Rem U) }
  | I64_REM_U; { I_binop (S64, Rem U) }
  | I32_AND; { I_binop (S32, And) }
  | I64_AND; { I_binop (S64, And) }
  | I32_OR; { I_binop (S32, Or) }
  | I64_OR; { I_binop (S64, Or) }
  | I32_XOR; { I_binop (S32, Xor) }
  | I64_XOR; { I_binop (S64, Xor) }
  | I32_SHL; { I_binop (S32, Shl) }
  | I64_SHL; { I_binop (S64, Shl) }
  | I32_SHR_S; { I_binop (S32, Shr S) }
  | I64_SHR_S; { I_binop (S64, Shr S) }
  | I32_SHR_U; { I_binop (S32, Shr U) }
  | I64_SHR_U; { I_binop (S64, Shr U) }
  | I32_ROTL; { I_binop (S32, Rotl) }
  | I64_ROTL; { I_binop (S64, Rotl) }
  | I32_ROTR; { I_binop (S32, Rotr) }
  | I64_ROTR; { I_binop (S64, Rotr) }
  | F32_ADD; { F_binop (S32, Add) }
  | F64_ADD; { F_binop (S64, Add) }
  | F32_SUB; { F_binop (S32, Sub) }
  | F64_SUB; { F_binop (S64, Sub) }
  | F32_MUL; { F_binop (S32, Mul) }
  | F64_MUL; { F_binop (S64, Mul) }
  | F32_DIV; { F_binop (S32, Div) }
  | F64_DIV; { F_binop (S64, Div) }
  | F32_MIN; { F_binop (S32, Min) }
  | F64_MIN; { F_binop (S64, Min) }
  | F32_MAX; { F_binop (S32, Max) }
  | F64_MAX; { F_binop (S64, Max) }
  | F32_COPYSIGN; { F_binop (S32, Copysign) }
  | F64_COPYSIGN; { F_binop (S64, Copysign) }
  | I32_EQZ; { I_testop (S32, Eqz) }
  | I64_EQZ; { I_testop (S64, Eqz) }
  | I32_EQ; { I_relop (S32, Eq) }
  | I64_EQ; { I_relop (S64, Eq) }
  | I32_NE; { I_relop (S32, Ne) }
  | I64_NE; { I_relop (S64, Ne) }
  | I32_LT_S; { I_relop (S32, Lt S) }
  | I64_LT_S; { I_relop (S64, Lt S) }
  | I32_LT_U; { I_relop (S32, Lt U) }
  | I64_LT_U; { I_relop (S64, Lt U) }
  | I32_GT_S; { I_relop (S32, Gt S) }
  | I64_GT_S; { I_relop (S64, Gt S) }
  | I32_GT_U; { I_relop (S32, Gt U) }
  | I64_GT_U; { I_relop (S64, Gt U) }
  | I32_LE_S; { I_relop (S32, Le S) }
  | I64_LE_S; { I_relop (S64, Le S) }
  | I32_LE_U; { I_relop (S32, Le U) }
  | I64_LE_U; { I_relop (S64, Le U) }
  | I32_GE_S; { I_relop (S32, Ge S) }
  | I64_GE_S; { I_relop (S64, Ge S) }
  | I32_GE_U; { I_relop (S32, Ge U) }
  | I64_GE_U; { I_relop (S64, Ge U) }
  | F32_EQ; { F_relop (S32, Eq) }
  | F64_EQ; { F_relop (S64, Eq) }
  | F32_NE; { F_relop (S32, Ne) }
  | F64_NE; { F_relop (S64, Ne) }
  | F32_LT; { F_relop (S32, Lt) }
  | F64_LT; { F_relop (S64, Lt) }
  | F32_GT; { F_relop (S32, Gt) }
  | F64_GT; { F_relop (S64, Gt) }
  | F32_LE; { F_relop (S32, Le) }
  | F64_LE; { F_relop (S64, Le) }
  | F32_GE; { F_relop (S32, Ge) }
  | F64_GE; { F_relop (S64, Ge) }
  | I32_EXTEND8_S; { I_extend8_s S32 }
  | I64_EXTEND8_S; { I_extend8_s S64 }
  | I32_EXTEND16_S; { I_extend16_s S32 }
  | I64_EXTEND16_S; { I_extend16_s S64 }
  | I64_EXTEND32_S; { I64_extend32_s }
  | I32_WRAP_I64; { I32_wrap_i64 }
  | I64_EXTEND_I32_S; { I64_extend_i32 S }
  | I64_EXTEND_I32_U; { I64_extend_i32 U }
  | I32_TRUNC_F32_S; { I_trunc_f (S32, S32, S) }
  | I32_TRUNC_F32_U; { I_trunc_f (S32, S32, U) }
  | I32_TRUNC_F64_S; { I_trunc_f (S32, S64, S) }
  | I32_TRUNC_F64_U; { I_trunc_f (S32, S64, U) }
  | I64_TRUNC_F32_S; { I_trunc_f (S64, S32, S) }
  | I64_TRUNC_F32_U; { I_trunc_f (S64, S32, U) }
  | I64_TRUNC_F64_S; { I_trunc_f (S64, S64, S) }
  | I64_TRUNC_F64_U; { I_trunc_f (S64, S64, U) }
  | I32_TRUNC_SAT_F32_S; { I_trunc_sat_f (S32, S32, S) }
  | I32_TRUNC_SAT_F32_U; { I_trunc_sat_f (S32, S32, U) }
  | I32_TRUNC_SAT_F64_S; { I_trunc_sat_f (S32, S64, S) }
  | I32_TRUNC_SAT_F64_U; { I_trunc_sat_f (S32, S64, U) }
  | I64_TRUNC_SAT_F32_S; { I_trunc_sat_f (S64, S32, S) }
  | I64_TRUNC_SAT_F32_U; { I_trunc_sat_f (S64, S32, U) }
  | I64_TRUNC_SAT_F64_S; { I_trunc_sat_f (S64, S64, S) }
  | I64_TRUNC_SAT_F64_U; { I_trunc_sat_f (S64, S64, U) }
  | F32_DEMOTE_F64; { F32_demote_f64 }
  | F64_PROMOTE_F32; { F64_promote_f32 }
  | F32_CONVERT_I32_S; { F_convert_i (S32, S32, S) }
  | F32_CONVERT_I32_U; { F_convert_i (S32, S32, U) }
  | F32_CONVERT_I64_S; { F_convert_i (S32, S64, S) }
  | F32_CONVERT_I64_U; { F_convert_i (S32, S64, U) }
  | F64_CONVERT_I32_S; { F_convert_i (S64, S32, S) }
  | F64_CONVERT_I32_U; { F_convert_i (S64, S32, U) }
  | F64_CONVERT_I64_S; { F_convert_i (S64, S64, S) }
  | F64_CONVERT_I64_U; { F_convert_i (S64, S64, U) }  
  | I32_REINTERPRET_F32; { I_reinterpret_f (S32, S32) }
  | I32_REINTERPRET_F64; { I_reinterpret_f (S32, S64) }
  | I64_REINTERPRET_F32; { I_reinterpret_f (S64, S32) }
  | I64_REINTERPRET_F64; { I_reinterpret_f (S64, S64) }
  | F32_REINTERPRET_I32; { F_reinterpret_i (S32, S32) }
  | F32_REINTERPRET_I64; { F_reinterpret_i (S32, S64) }
  | F64_REINTERPRET_I32; { F_reinterpret_i (S64, S32) }
  | F64_REINTERPRET_I64; { F_reinterpret_i (S64, S64) }
  | REF_NULL; ~ = ref_kind; <Ref_null>
  | REF_IS_NULL; { Ref_is_null }
  | REF_FUNC; ~ = indice; <Ref_func>
  | I32_LOAD; memarg = memarg; { I_load (S32, memarg) }
  | I64_LOAD; memarg = memarg; { I_load (S64, memarg) }
  | F32_LOAD; memarg = memarg; { F_load (S32, memarg) }
  | F64_LOAD; memarg = memarg; { F_load (S64, memarg) }
  | I32_STORE; memarg = memarg; { I_store (S32, memarg) }
  | I64_STORE; memarg = memarg; { I_store (S64, memarg) }
  | F32_STORE; memarg = memarg; { F_store (S32, memarg) }
  | F64_STORE; memarg = memarg; { F_store (S64, memarg) }
  | I32_LOAD8_S; memarg = memarg; { I_load8 (S32, S, memarg ) }
  | I32_LOAD8_U; memarg = memarg; { I_load8 (S32, U, memarg ) }
  | I64_LOAD8_S; memarg = memarg; { I_load8 (S64, S, memarg ) }
  | I64_LOAD8_U; memarg = memarg; { I_load8 (S64, U, memarg ) }
  | I32_LOAD16_S; memarg = memarg; { I_load16 (S32, S, memarg )  }
  | I32_LOAD16_U; memarg = memarg; { I_load16 (S32, U, memarg ) }
  | I64_LOAD16_S; memarg = memarg; { I_load16 (S64, S, memarg ) }
  | I64_LOAD16_U; memarg = memarg; { I_load16 (S64, U, memarg ) }
  | I64_LOAD32_S; memarg = memarg; { I64_load32 (S, memarg) }
  | I64_LOAD32_U; memarg = memarg; { I64_load32 (U, memarg) }
  | I32_STORE8; memarg = memarg; { I_store8 (S32, memarg) }
  | I64_STORE8; memarg = memarg; { I_store8 (S64, memarg) }
  | I32_STORE16; memarg = memarg; { I_store16 (S32, memarg) }
  | I64_STORE16; memarg = memarg; { I_store16 (S64, memarg) }
  | I64_STORE32; ~ = memarg; <I64_store32>
  | MEMORY_SIZE; { Memory_size }
  | MEMORY_GROW; { Memory_grow }
  | MEMORY_FILL; { Memory_fill }
  | MEMORY_COPY; { Memory_copy }
  | MEMORY_INIT; ~ = indice; <Memory_init>
  | DATA_DROP; ~ = indice; <Data_drop>

(* Instructions & Expressions *)

let select_instr ==
  | SELECT; (b, ts) = select_instr_results; {
    Select (if b then (Some ts) else None)
  }

let select_instr_results :=
  | LPAR; RESULT; l = list(val_type); RPAR; (_, ts) = select_instr_results; {
    true, l @ ts
  }
  | {false, []}

let select_instr_instr ==
  | SELECT; (b, ts, es) = select_instr_results_instr; {
    Select (if b then Some ts else None), es
  }

let select_instr_results_instr :=
  | LPAR; RESULT; l = list(val_type); RPAR; (_, ts, es) = select_instr_results_instr; {
    true, l @ ts, es
  }
  | ~ = instr; {
    false, [], instr
  }

let call_instr ==
  | CALL_INDIRECT; ~ = id; ~ = call_instr_type; {
    Call_indirect (Symbolic id, call_instr_type)
  }
  | CALL_INDIRECT; ~ = call_instr_type; {
    Call_indirect (Raw 0, call_instr_type)
  }

let call_instr_type ==
  | ~ = type_use; ~ = call_instr_params; {
    match call_instr_params with
    | ([], []) -> Bt_ind (type_use)
    | (pt, rt) -> Bt_raw (Some type_use, (List.map (fun t -> None, t) pt, rt))
  }
  | (pt, rt) = call_instr_params; { Bt_raw (None, (List.map (fun t -> None, t) pt, rt)) }

let call_instr_params :=
  | LPAR; PARAM; l = list(val_type); RPAR; (ts1, ts2) = call_instr_params; {
    l @ ts1, ts2
  }
  | ~ = call_instr_results; {
    [], call_instr_results
  }

let call_instr_results :=
  | LPAR; RESULT; l = list(val_type); RPAR; ~ = call_instr_results; {
    l @ call_instr_results
  }
  | { [] }

let call_instr_instr ==
  | CALL_INDIRECT; ~ = id; (x, es) = call_instr_type_instr; {
    Call_indirect (Symbolic id, x), es
  }
  | CALL_INDIRECT; (x, es) = call_instr_type_instr; {
    Call_indirect (Raw 0, x), es
  }

let call_instr_type_instr ==
  | ~ = type_use; ~ = call_instr_params_instr; {
    match call_instr_params_instr with
    | ([], []), es -> Bt_ind (type_use), es
    | (pt, rt), es -> Bt_raw (Some type_use, ((List.map (fun t -> None, t) pt), rt)), es
  }
  | ((pt, rt), es) = call_instr_params_instr; { Bt_raw (None, (List.map (fun t -> None, t) pt, rt)), es }

let call_instr_params_instr :=
  | LPAR; PARAM; l = list(val_type); RPAR; ((ts1, ts2), es) = call_instr_params_instr; {
    (l @ ts1, ts2), es
  }
  | (ts, es) = call_instr_results_instr; {
    ([], ts), es
  }

let call_instr_results_instr :=
  | LPAR; RESULT; l = list(val_type); RPAR; (ts, es) = call_instr_results_instr; {
    l @ ts, es
  }
  | ~ = instr; {
    [], instr
  }

let block_instr ==
  | BLOCK; id = option(id); (bt, es) = block; END; id2 = option(id); {
    if Option.is_some id2 && id <> id2 then failwith "mismatching label";
    Block (id, bt, es)
  }
  | LOOP; id = option(id); (bt, es) = block; END; id2 = option(id); {
    if Option.is_some id2 && id <> id2 then failwith "mismatching label";
    Loop (id, bt, es)
  }
  | IF; id = option(id); (bt, es) = block; END; id2 = option(id); {
    if Option.is_some id2 && id <> id2 then failwith "mismatching label";
    If_else (id, bt, es, [])
  }
  | IF; id = option(id); (bt, es1) = block; ELSE; id2 = option(id); ~ = instr_list; END; id3 = option(id); {
    if Option.is_some id2 && id <> id2 then failwith "mismatching label";
    if Option.is_some id3 && id <> id3 then failwith "mismatching label";
    If_else (id, bt, es1, instr_list)
  }

let block ==
  | ~ = type_use; (l, r) = block_param_body; {
    let block_type = match l with
    | ([], []) -> Bt_ind type_use
    | (pt, rt) -> Bt_raw (Some type_use, (List.map (fun t -> None, t) pt, rt))
    in
    Some block_type, r
  }
  | (l, r) = block_param_body; {
    let block_type = match l with
      | [], [] -> None
      | (pt, rt) -> Some (Bt_raw (None, (List.map (fun t -> None, t) pt, rt)))
    in
    block_type, r
  }


let block_param_body :=
  | ~ = block_result_body; <>
  | LPAR; PARAM; l = list(val_type); RPAR; ((ins, out), instr_list) = block_param_body; {
    (l @ ins, out), instr_list
  }

let block_result_body :=
  | ~ = instr_list; { ([], []), instr_list }
  | LPAR; RESULT; l = list(val_type); RPAR; ((ins, out), instr_list) = block_result_body; {
    (ins, l @out), instr_list
  }

let expr_aux ==
  | ~ = plain_instr; ~ = expr_list; { expr_list, plain_instr }
  | SELECT; (b, ts, es) = select_expr_result; {
    es, Select (if b then Some ts else None)
  }
  | CALL_INDIRECT; ~ = id; (x, es) = call_expr_type; {
    es, Call_indirect (Symbolic id, x)
  }
  | CALL_INDIRECT; (x, es) = call_expr_type; {
    es, Call_indirect (Raw 0, x)
  }
  | BLOCK; id = option(id); (bt, es) = block; {
    [], Block (id, bt, es)
  }
  | LOOP; id = option(id); (bt, es) = block; {
    [], Loop (id, bt, es)
  }
  | IF; id = option(id); (bt, ((pt, rt), (es, es1, es2))) = if_block; {
    let bt = match pt, rt with
    | [], [] -> bt
    | pt, rt ->
      let pt = List.map (fun t -> None, t) pt in
      let raw = pt, rt in
      begin match bt with
      | Some (Bt_ind type_use) -> Some (Bt_raw (Some type_use, raw))
      | Some (Bt_raw _) -> failwith "unexpected bt_raw"
      | None -> Some (Bt_raw (None, raw))
      end
    in
    es, If_else (id, bt, es1, es2)
  }

let expr :=
  | (es, e) = par(expr_aux); {
    es @ [e]
  }

let select_expr_result :=
  | LPAR; RESULT; l = list(val_type); RPAR; (_, ts, es) = select_expr_result; {
    true, l @ ts, es
  }
  | ~ = expr_list; { false, [], expr_list }

let call_expr_type ==
  | ~ = type_use; ~ = call_expr_params; {
    match call_expr_params with
    | ([], []), es -> Bt_ind type_use, es
    | (pt, rt), es -> Bt_raw (Some type_use, (List.map (fun t -> None, t) pt, rt)), es
  }
  | ((pt, rt), es) = call_expr_params; {
    Bt_raw (None, (List.map (fun t -> None, t) pt, rt)), es
  }

let call_expr_params :=
  | LPAR; PARAM; l = list(val_type); RPAR; ((ts1, ts2), es) = call_expr_params; {
    (l @ ts1, ts2), es
  }
  | (ts, es) = call_expr_results; {
    ([], ts), es
  }

let call_expr_results :=
  | LPAR; RESULT; l = list(val_type); RPAR; (ts, es) = call_expr_results; {
    l @ ts, es
  }
  | ~ = expr_list; { [], expr_list }

let if_block ==
  | ~ = type_use; ~ = if_block_param_body; { Some (Bt_ind type_use), if_block_param_body }
  | ~ = if_block_param_body; {
    None, if_block_param_body
  }

let if_block_param_body :=
  | ~ = if_block_result_body; <>
  | LPAR; PARAM; l = list(val_type); RPAR; ((ins, out), if_) = if_block_param_body; {
    (l @ ins, out), if_
  }

let if_block_result_body :=
  | ~ = if_; { ([], []), if_ }
  | LPAR; RESULT; l = list(val_type); RPAR; ((ins, out), if_) = if_block_result_body; {
    (ins, l @ out), if_
  }

let if_ :=
  | ~ = expr; (es0, es1, es2) = if_; {
    expr @ es0, es1, es2
  }
  | LPAR; THEN; es1 = instr_list; RPAR; LPAR; ELSE; es2 = instr_list; RPAR; {
    [], es1, es2
  }
  | LPAR; THEN; ~ = instr_list; RPAR; {
    [], instr_list, []
  }

let instr_list :=
  | { [] }
  | ~ = select_instr; { [select_instr] }
  | ~ = call_instr; { [call_instr] }
  | ~ = instr; ~ = instr_list; { instr @ instr_list }

let expr_list :=
  | { [] }
  | ~ = expr; ~ = expr_list; { expr @ expr_list }

let const_expr ==
  | ~ = instr_list; <>

(* Functions *)

let func ==
  | FUNC; id = option(id); ~ = func_fields; {
    let func_id = Option.map (fun id -> Symbolic id) id in
    List.rev_map (function
      | MImport i ->
        begin match i.desc with
        | Import_func (_id, ft) -> MImport { i with desc = Import_func (id, ft) }
        | _ -> assert false
        end
      | MExport e -> MExport { e with desc = Export_func func_id }
      | MFunc f -> MFunc { f with id }
      | _field -> (Format.eprintf "got invalid field: `%a`@." Pp.Input.module_field _field; assert false)
    ) func_fields
  }

let func_fields :=
  | ~ = type_use; (ft, f) = func_fields_body; {
    match ft with
    | [], [] -> [MFunc { f with type_f = Bt_ind type_use }]
    | (_pt, _rt) as ft -> [MFunc { f with type_f = Bt_raw (Some type_use, ft)}]
  }
  | (type_f, f) = func_fields_body; {
    [MFunc { f with type_f = Bt_raw (None, type_f) }]
  }
  | (module_, name) = inline_import; ~ = type_use; _ = func_fields_import; {
    [MImport { module_; name; desc = Import_func (None, Bt_ind type_use) }]
  }
  | (module_, name) = inline_import; ~ = func_fields_import; {
    [MImport { module_; name; desc = Import_func (None, Bt_raw (None, func_fields_import)) }]
  }
  | ~ = inline_export; ~ = func_fields; {
    MExport { name = inline_export; desc = Export_func None } :: func_fields
  }

let func_fields_import :=
  | ~ = func_fields_import_result; <>
  | LPAR; PARAM; ins = list(val_type); RPAR; (ins2, out) = func_fields_import; {
    (List.map (fun i -> None, i) ins) @ ins2, out
  }
  | LPAR; PARAM; ~ = id; ~ = val_type; RPAR; (ins2, out) = func_fields_import; {
    (Some id, val_type) :: ins2, out
  }

let func_fields_import_result :=
  | { [], [] }
  | LPAR; RESULT; l = list(val_type); RPAR; (ins, out) = func_fields_import_result; {
    ins, l @ out
  }

let func_fields_body :=
  | ~ = func_result_body; <>
  | LPAR; PARAM; l = list(val_type); RPAR; ((ins, out), r) = func_fields_body; {
    ((List.map (fun i -> None, i) l) @ ins, out), r
  }
  | LPAR; PARAM; ~ = id; ~ = val_type; RPAR; ((ins, out), r) = func_fields_body; {
    ((Some id, val_type) :: ins, out), r
  }

let func_result_body :=
  | ~ = func_body; { ([], []), func_body }
  | LPAR; RESULT; l = list(val_type); RPAR; ((ins, out), r) = func_result_body; {
    (ins, l @ out), r
  }

let func_body :=
  | body = instr_list; {
    { type_f = Bt_ind (Raw Int.max_int); locals = []; body; id = None }
  }
  | LPAR; LOCAL; l = list(val_type); RPAR; ~ = func_body; {
    { func_body with locals = (List.map (fun v -> None, v) l) @ func_body.locals  }
  }
  | LPAR; LOCAL; ~ = id; ~ = val_type; RPAR; ~ = func_body; {
    { func_body with locals = (Some id, val_type) :: func_body.locals }
  }

(* Tables, Memories & Globals *)

let table_use ==
  | TABLE; ~ = indice; <>

let memory_use ==
  | MEMORY; ~ = indice; <>

let offset ==
  | LPAR; OFFSET; ~ = const_expr; RPAR; <>
  | ~ = expr; <>

let elem_kind ==
  | FUNC; { Func_ref }

let elem_expr ==
  | LPAR; ITEM; ~ = const_expr; RPAR; <>
  | ~ = expr; <>

let elem_var ==
  | ~ = indice; <Ref_func>

let elem_list ==
  | ~ = elem_kind; l = list(elem_var); { elem_kind, [l] }
  | ~ = ref_type; l = list(elem_expr); { ref_type, l }

let elem ==
  | ELEM; id = option(id); (type_, init) = elem_list; {
    [ MElem { id; type_; init; mode = Elem_passive } ]
  }
  | ELEM; id = option(id); table_use = par(table_use); ~ = offset; (type_, init) = elem_list; {
    [ MElem { id; type_; init; mode = Elem_active (Some table_use, offset) } ]
  }
  | ELEM; id = option(id); DECLARE; (type_, init) = elem_list; {
    [ MElem { id; type_; init; mode = Elem_declarative } ]
  }
  | ELEM; id = option(id); ~ = offset; (type_, init) = elem_list; {
    [ MElem { id; type_; init; mode = Elem_active (Some (Raw 0), offset) } ]
  }
  | ELEM; id = option(id); ~ = offset; init = list(elem_var); {
    [ MElem { id; type_ = Func_ref; init = [init]; mode = Elem_active (Some (Raw 0), offset) } ]
  }

let table ==
| TABLE; id = option(id); ~ = table_fields; {
  let tbl_id = Option.map (fun id -> Symbolic id) id in
  List.rev_map (function
    | MTable (_id, tbl) -> MTable (id, tbl)
    | MExport e -> MExport { e with desc = Export_table tbl_id }
    | MElem e -> MElem { e with mode = Elem_active (tbl_id, [I32_const 0l]) }
    | MImport i -> begin match i.desc with
      | Import_table (_id, table_type) -> MImport { i with desc = Import_table (id, table_type) }
      | _whatever -> assert false
    end
    | _field -> (Format.eprintf "got invalid field: `%a`@." Pp.Input.module_field _field; assert false)
  ) table_fields
}

let init ==
  | l = list(elem_var); { [l] }
  | ~ = nonempty_list(elem_expr); <>

let table_fields :=
  | ~ = table_type; {
    [ MTable (None, table_type) ]
  }
  | (module_, name) = inline_import; ~ = table_type; {
    [ MImport { module_; name; desc = Import_table (None, table_type) }]
  }
  | ~ = inline_export; ~ = table_fields; {
    MExport { name = inline_export; desc = Export_table None } :: table_fields
  }
  | ~ = ref_type; LPAR; ELEM; ~ = init; RPAR; {
    let min = List.length (List.flatten init) in
    [ MElem { id = None; type_ = Func_ref; init; mode = Elem_active (None, []) }
    ; MTable (None, ({ min; max = Some min }, ref_type)) ]
  }

let data ==
  | DATA; id = option(id); init = string_list; {
    [ MData { id; init; mode = Data_passive } ]
  }
  | DATA; id = option(id); memory_use = ioption(par(memory_use)); ~ = offset; init = string_list; {
    let memory_use = Option.value memory_use ~default:(Raw 0) in
    [ MData { id; init; mode = Data_active (Some memory_use, offset) } ]
  }

let memory ==
  | MEMORY; id = option(id); ~ = memory_fields; {
    let mem_id = Option.map (fun id -> Symbolic id) id in
    List.rev_map (function
      | MMem (_id, m) -> MMem (id, m)
      | MExport e -> MExport { e with desc = Export_mem mem_id }
      | MData d -> MData { d with mode = Data_active (mem_id, [I32_const 0l]) }
      | MImport i -> begin match i.desc with
        | Import_mem (_id, mem_type ) -> MImport { i with desc = Import_mem (id, mem_type) }
        | _whatever -> assert false
        end
      | _field -> (Format.eprintf "got invalid field: `%a`@." Pp.Input.module_field _field; assert false)
    ) memory_fields
  }

let memory_fields :=
  | ~ = mem_type; {
    [ MMem (None, mem_type) ]
  }
  | (module_, name) = inline_import; ~ = mem_type; {
    [ MImport { module_; name; desc = Import_mem (None, mem_type) } ]
  }
  | ~ = inline_export; ~ = memory_fields; {
    MExport { name = inline_export; desc = Export_mem None } :: memory_fields
  }
  | LPAR; DATA; init = string_list; RPAR; {
    let min = Int32.(div (add (of_int (String.length init)) 65535l) 65536l) in
    let min = Int32.to_int min in
    
    [ MData { id = None; init; mode = Data_active (None, []) } 
    ; MMem (None, { min; max = Some min}) ]
  }

let global ==
  | GLOBAL; id = option(id); ~ = global_fields; {
    let global_id = Option.map (fun id -> Symbolic id) id in
    List.rev_map (function
      | MGlobal g -> MGlobal { g with id }
      | MExport e -> MExport { e with desc = Export_global global_id }
      | MImport i ->
        begin match i.desc with
        | Import_global (_id, t) -> MImport { i with desc = Import_global (id, t) }
        | _ -> assert false
        end
      | _field -> (Format.eprintf "got invalid field: `%a`@." Pp.Input.module_field _field; assert false)
    ) global_fields
  }

let global_fields :=
  | type_ = global_type; init = const_expr; {
    [ MGlobal { type_; init; id = None } ]
  }
  | (module_, name) = inline_import; ~ = global_type; {
    [ MImport { module_; name; desc = Import_global (None, global_type) } ]
  }
  | ~ = inline_export; ~ = global_fields; {
    MExport { name = inline_export; desc = Export_global None } :: global_fields
  }

(* Imports & Exports *)

let import_desc ==
  | FUNC; id = option(id); ~ = type_use; { Import_func (id, Bt_ind type_use) }
  | (id, ft) = preceded(FUNC, pair(option(id), func_type)); { Import_func (id, Bt_raw (None, ft)) }
  | TABLE; ~ = option(id); ~ = table_type; <Import_table>
  | MEMORY; ~ = option(id); ~ = mem_type; <Import_mem>
  | GLOBAL; ~ = option(id); ~ = global_type; <Import_global>

let import ==
  | IMPORT; module_ = utf8_name; name = utf8_name; desc = par(import_desc); {
    [ MImport { module_; name; desc } ]
  }

let inline_import ==
  | LPAR; IMPORT; ln = utf8_name; rn = utf8_name; RPAR; { ln, rn }

let export_desc ==
  | FUNC; ~ = indice; { Export_func (Some indice) }
  | TABLE; ~ = indice; { Export_table (Some indice) }
  | MEMORY; ~ = indice; { Export_mem (Some indice) }
  | GLOBAL; ~ = indice; { Export_global (Some indice) }

let export ==
  | EXPORT; name = utf8_name; desc = par(export_desc); {
    [ MExport { name; desc; } ]
  }

let inline_export ==
  | LPAR; EXPORT; ~ = utf8_name; RPAR;  <>

(* Modules *)

let type_ ==
  | ~ = def_type; <>

let type_def ==
  | TYPE; id = option(id); ~ = type_; { [ MType (id, type_) ] }

let start ==
  | START; ~ = indice; { [ MStart indice ] }

let module_field :=
  | ~ = type_def; <>
  | ~ = start; <>
  | ~ = export; <>
  | ~ = import; <>
  | ~ = elem; <>
  | ~ = data; <>
  | ~ = func; <>
  | ~ = global; <>
  | ~ = table; <>
  | ~ = memory; <>

let module_ :=
  | MODULE; id = ioption(id); fields = list(par(module_field)); {
    (* TODO: handle fields_bin
    let fields_bin = String.concat "" l in *)
    let fields = List.flatten fields in
    { id; fields }
  }

let module_binary :=
  | MODULE; id = ioption(id); BINARY; fields = list(par(module_field)); _ = list(NAME); {
    (* TODO: handle fields_bin
    let fields_bin = String.concat "" l in *)
    let fields = List.flatten fields in
    { id; fields }
  }

let literal_const ==
  | I32_CONST; num = NUM; { Const_I32 (i32 num) }
  | I64_CONST; num = NUM; { Const_I64 (i64 num) }
  | F32_CONST; num = NUM; { Const_F32 (f32 num) }
  | F64_CONST; num = NUM; { Const_F64 (f64 num) }
  | REF_NULL; ~ = ref_kind; <Const_null>
  | REF_EXTERN; num = NUM; { Const_host (int_of_string num) }

let const ==
  | ~ = literal_const; <Literal>
  | F32_CONST; NAN_CANON; { Nan_canon S32 }
  | F64_CONST; NAN_CANON; { Nan_canon S64 }
  | F32_CONST; NAN_ARITH; { Nan_arith S32 }
  | F64_CONST; NAN_ARITH; { Nan_arith S64 }

let result ==
  | ~ = const; <Result_const>

let assert_ ==
  | ASSERT_RETURN; ~ = par(action); ~ = list(par(result)); <Assert_return>
  | ASSERT_EXHAUSTION; ~ = par(action); ~ = NAME; <Assert_exhaustion>
  | ASSERT_TRAP; ~ = par(action); ~ = NAME; <Assert_trap>
  | ASSERT_TRAP; ~ = par(module_); ~ = NAME; <Assert_trap_module>
  | ASSERT_MALFORMED; ~ = par(module_); ~ = NAME; <Assert_malformed>
  | ASSERT_MALFORMED; LPAR; MODULE; QUOTE; ~ = list(NAME); RPAR; ~ = NAME; <Assert_malformed_quote>
  | ASSERT_MALFORMED; LPAR; MODULE; BINARY; ~ = list(NAME); RPAR; ~ = NAME; <Assert_malformed_binary>
  | ASSERT_INVALID; ~ = par(module_); ~ = NAME; <Assert_invalid>
  | ASSERT_INVALID; LPAR; MODULE; QUOTE; ~ = list(NAME); RPAR; ~ = NAME; <Assert_invalid_quote>
  | ASSERT_INVALID; LPAR; MODULE; BINARY; ~ = list(NAME); RPAR; ~ = NAME; <Assert_invalid_binary>
  | ASSERT_UNLINKABLE; ~ = par(module_); ~ = NAME; <Assert_unlinkable>

let register ==
  | REGISTER; ~ = utf8_name; ~ = option(id); <Register>

let action ==
  | INVOKE; ~ = ioption(id); ~ = utf8_name; ~ = list(par(literal_const)); <Invoke>
  | GET; ~ = ioption(id); ~ = utf8_name; <Get>

let cmd ==
  | ~ = par(module_); <Module>
  | ~ = par(module_binary); <Module>
  | ~ = par(assert_); <Assert>
  | ~ = par(register); <>
  | ~ = par(action); <Action>

let file :=
  | ~ = nonempty_list(cmd); EOF; <>
  | fields = nonempty_list(par(module_field)); EOF; {
    let fields = List.flatten fields in
    let id = None in
    let module_ = { id; fields } in
    [ Module module_ ]
  }
