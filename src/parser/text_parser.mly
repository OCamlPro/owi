%token <String.t> NUM
%token <String.t> ID NAME
%token ALIGN ANY_REF ARRAY ARRAY_GET ARRAY_GET_U ARRAY_LEN ARRAY_NEW_CANON ARRAY_NEW_CANON_DATA ARRAY_NEW_CANON_DEFAULT ARRAY_NEW_CANON_ELEM ARRAY_NEW_CANON_FIXED ARRAY_REF ARRAY_SET ASSERT_EXHAUSTION ASSERT_INVALID ASSERT_MALFORMED ASSERT_RETURN ASSERT_TRAP ASSERT_UNLINKABLE ANY
%token BINARY BLOCK BR BR_IF BR_ON_CAST BR_ON_CAST_FAIL BR_ON_NON_NULL BR_ON_NULL BR_TABLE
%token CALL CALL_INDIRECT CALL_REF
%token DATA DATA_DROP DECLARE DROP
%token ELEM ELEM_DROP ELSE END EOF EQ EQ_REF EQUAL EXPORT EXTERN EXTERN_EXTERNALIZE EXTERN_INTERNALIZE EXTERN_REF
%token F32 F32_ABS F32_ADD F32_CEIL F32_CONST F32_CONVERT_I32_S F32_CONVERT_I32_U F32_CONVERT_I64_S F32_CONVERT_I64_U F32_COPYSIGN F32_DEMOTE_F64 F32_DIV F32_EQ F32_FLOOR F32_GE F32_GT F32_LE F32_LOAD F32_LT F32_MAX F32_MIN F32_MUL F32_NE F32_NEAREST F32_NEG F32_REINTERPRET_I32 F32_REINTERPRET_I64 F32_SQRT F32_STORE F32_SUB F32_TRUNC
%token F64 F64_ABS F64_ADD F64_CEIL F64_CONST F64_CONVERT_I32_S F64_CONVERT_I32_U F64_CONVERT_I64_S F64_CONVERT_I64_U F64_COPYSIGN F64_DIV F64_EQ F64_FLOOR F64_GE F64_GT F64_LE F64_LOAD F64_LT F64_MAX F64_MIN F64_MUL F64_NE F64_NEAREST F64_NEG F64_PROMOTE_F32 F64_REINTERPRET_I32 F64_REINTERPRET_I64 F64_SQRT F64_STORE F64_SUB F64_TRUNC
%token FIELD FINAL FUNC FUNC_REF
%token GET GLOBAL GLOBAL_GET GLOBAL_SET
%token I16 I31 I31_GET_S I31_GET_U I31_REF
%token I32 I32_ADD I32_AND I32_CLZ I32_CONST I32_CTZ I32_DIV_S I32_DIV_U I32_EQ I32_EQZ I32_EXTEND16_S I32_EXTEND8_S I32_GE_S I32_GE_U I32_GT_S I32_GT_U I32_LE_S I32_LE_U I32_LOAD I32_LOAD16_S I32_LOAD16_U I32_LOAD8_S I32_LOAD8_U I32_LT_S I32_LT_U I32_MUL I32_NE I32_OR I32_POPCNT I32_REINTERPRET_F32 I32_REINTERPRET_F64 I32_REM_S I32_REM_U I32_ROTL I32_ROTR I32_SHL I32_SHR_S I32_SHR_U I32_STORE I32_STORE16 I32_STORE8 I32_SUB I32_TRUNC_F32_S I32_TRUNC_F32_U I32_TRUNC_F64_S I32_TRUNC_F64_U I32_TRUNC_SAT_F32_S I32_TRUNC_SAT_F32_U I32_TRUNC_SAT_F64_S I32_TRUNC_SAT_F64_U I32_WRAP_I64 I32_XOR
%token I64 I64_ADD I64_AND I64_CLZ I64_CONST I64_CTZ I64_DIV_S I64_DIV_U I64_EQ I64_EQZ I64_EXTEND16_S I64_EXTEND32_S I64_EXTEND8_S I64_EXTEND_I32_S I64_EXTEND_I32_U I64_GE_S I64_GE_U I64_GT_S I64_GT_U I64_LE_S I64_LE_U I64_LOAD I64_LOAD16_S I64_LOAD16_U I64_LOAD32_S I64_LOAD32_U I64_LOAD8_S I64_LOAD8_U I64_LT_S I64_LT_U I64_MUL I64_NE I64_OR I64_POPCNT I64_REINTERPRET_F32 I64_REINTERPRET_F64 I64_REM_S I64_REM_U I64_ROTL I64_ROTR I64_SHL I64_SHR_S I64_SHR_U I64_STORE I64_STORE16 I64_STORE32 I64_STORE8 I64_SUB I64_TRUNC_F32_S I64_TRUNC_F32_U I64_TRUNC_F64_S I64_TRUNC_F64_U I64_TRUNC_SAT_F32_S I64_TRUNC_SAT_F32_U I64_TRUNC_SAT_F64_S I64_TRUNC_SAT_F64_U I64_XOR
%token V128 V128_CONST I8X16 I16X8 I32X4 I64X2 F32X4 F64X2 I32X4_ADD I64X2_ADD I32X4_SUB I64X2_SUB
%token I8 IF IMPORT INVOKE ITEM
%token LOCAL LOCAL_GET LOCAL_SET LOCAL_TEE LOOP LPAR
%token MEMORY MEMORY_COPY MEMORY_FILL MEMORY_GROW MEMORY_INIT MEMORY_SIZE MODULE MUTABLE DEFINITION INSTANCE
%token NAN_ARITH NAN_CANON NOEXTERN NOFUNC NONE NOP NULL NULL_EXTERN_REF NULL_FUNC_REF NULL_REF
%token TAG
%token OFFSET
%token PARAM
%token QUOTE
%token REC REF REF_ARRAY REF_AS_NON_NULL REF_CAST REF_EQ REF_EXTERN REF_FUNC REF_HOST REF_I31 REF_IS_NULL REF_NULL REF_STRUCT REF_TEST REGISTER RESULT RETURN RETURN_CALL RETURN_CALL_INDIRECT RETURN_CALL_REF RPAR
%token SELECT START STRUCT STRUCT_GET STRUCT_GET_S STRUCT_NEW_CANON STRUCT_NEW_CANON_DEFAULT STRUCTREF STRUCT_SET SUB
%token TABLE TABLE_COPY TABLE_FILL TABLE_GET TABLE_GROW TABLE_INIT TABLE_SET TABLE_SIZE THEN TYPE
%token UNREACHABLE

%{

open Wast
open Text

let failwith msg = raise @@ Parse_fail msg

let u32 s =
  match Int32.of_string s with
  | None ->
    Fmt.kstr failwith "constant out of range %s" s
  | Some v ->
      match Int32.unsigned_to_int v with
      | None -> Fmt.kstr failwith "constant out of range %s" s
      | Some v -> v

let i8 s =
  let i =
    try Int32.of_string_exn s
    with Failure msg -> Fmt.kstr failwith "constant out of range %s (%s)" s msg
  in
  if Int32.gt i 0xFFl || Int32.lt i (-0x80l) then
    Fmt.kstr failwith "constant out of range %s" s
  else Int32.to_int i

let i16 s =
  let i =
    try Int32.of_string_exn s
    with Failure msg -> Fmt.kstr failwith "constant out of range %s (%s)" s msg
  in
  if Int32.gt i 0xFFFFl || Int32.lt i (-0x8000l) then
    Fmt.kstr failwith "constant out of range %s" s
  else Int32.to_int i

let i32 s =
  try Int32.of_string_exn s
  with Failure msg -> Fmt.kstr failwith "constant out of range %s (%s)" s msg

let i64 s =
  try Int64.of_string_exn s
  with Failure msg -> Fmt.kstr failwith "constant out of range %s (%s)" s msg

let f64 s =
  try Float64.of_string_exn s
  with Failure msg -> Fmt.kstr failwith "constant out of range %s (%s)" s msg

let f32 s =
  try Float32.of_string_exn s
  with Failure msg -> Fmt.kstr failwith "constant out of range %s (%s)" s msg

let wrong_number_of_lane_literals () =
  failwith "wrong number of lane literals"

let t2_of_v128_arg_list z = function
  | [a; b] -> z a, z b
  | _ -> wrong_number_of_lane_literals ()

let t4_of_v128_arg_list z = function
  | [a; b; c; d] -> z a, z b, z c, z d
  | _ -> wrong_number_of_lane_literals ()

let t8_of_v128_arg_list z = function
  | [a; b; c; d; e; f; g; h] -> z a, z b, z c, z d, z e, z f, z g, z h
  | _ -> wrong_number_of_lane_literals ()

let t16_of_v128_arg_list z = function
  | [a; b; c; d; e; f; g; h;
     i; j; k; l; m; n; o; p;] ->
     z a, z b, z c, z d, z e, z f, z g, z h,
     z i, z j, z k, z l, z m, z n, z o, z p
  | _ -> wrong_number_of_lane_literals ()

let get_id_def0 = function
  | None -> Raw 0
  | Some v -> v

%}

%start <Wast.script> script
%start <Text.Module.t> modul
%start <Text.Module.t> inline_module

%%

(* Helpers *)

let utf8_name ==
  | name = NAME; {
    match Wutf8.check_utf8 name with
    | Ok () -> name
    | Error _msg -> failwith "malformed UTF-8 encoding"
  }

let par(X) ==
  | LPAR; ~ = X; RPAR; <>

let string_list ==
  | l = list(NAME); { String.concat "" l }

(* Types *)

let null_opt ==
  | NULL; { Null }
  | { No_null }

let heap_type ==
  | FUNC; { Func_ht }
  | EXTERN; { Extern_ht }
  | ~ = indice; <TypeUse>

let ref_type ==
  | LPAR; REF; ~ = null_opt; ~ = heap_type; RPAR; <>
  | FUNC_REF; { Null, Func_ht }
  | EXTERN_REF; { Null, Extern_ht }

let packed_type :=
  | I8; { I8 }
  | I16; { I16 }

let val_type :=
  | ~ = num_type; <Num_type>
  | ~ = ref_type; <Ref_type>

let global_type ==
  | ~ = val_type; { Const, val_type }
  | val_type = par(preceded(MUTABLE, val_type)); { Var, val_type }

let type_def ==
    | TYPE; id = option(id); LPAR; FUNC; ~ = func_type; RPAR; { id, func_type }

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

(* var *)
let indice ==
  | id = ID; { Text id }
  | n = NUM; { Raw (u32 n) }

(* memory index *)
let memidx ==
 | idx = option(indice); { get_id_def0 idx }

(* bind_var *)
let id ==
  | ~ = ID; <>

let num_type ==
  | I32; { Text.I32 }
  | I64; { Text.I64 }
  | F32; { Text.F32 }
  | F64; { Text.F64 }
  | V128; { Text.V128 }

let align ==
  | ALIGN; EQUAL; n = NUM; {
    let n = i32 n in
    if Int32.eq n 0l || Int32.ne Int32.(logand n (sub n 1l)) 0l then failwith "alignment"
    else Int32.div n 2l
  }

let memarg_offset ==
  | OFFSET; EQUAL; ~ = NUM; <>

let memarg ==
  | offset = option(memarg_offset); align = option(align); {
    let offset = i32 @@ Option.value offset ~default:"0" in
    let align = Option.value align ~default:0l in
    {offset; align}
  }

let instr ==
| ~ = plain_instr; { [plain_instr] }
| ~ = block_instr; { [ block_instr ] }
| ~ = expr; { expr }

let v128_const :=
  | V128_CONST; I8X16; n = list(NUM); {
        let (n1, n2, n3, n4, n5, n6, n7, n8,
             n9, n10, n11, n12, n13, n14, n15, n16) =
          t16_of_v128_arg_list i8 n
        in
        Concrete_v128.of_i8x16 n1 n2 n3 n4 n5 n6 n7 n8 n9 n10 n11 n12 n13 n14 n15 n16
  }
  | V128_CONST; I16X8; n = list(NUM); {
        let (n1, n2, n3, n4, n5, n6, n7, n8) = t8_of_v128_arg_list i16 n in
        Concrete_v128.of_i16x8 n1 n2 n3 n4 n5 n6 n7 n8
  }
  | V128_CONST; F32X4; n = list(NUM); {
        let (n1, n2, n3, n4) = t4_of_v128_arg_list f32 n in
        Concrete_v128.of_f32x4 n1 n2 n3 n4
  }
  | V128_CONST; F64X2; n = list(NUM); {
        let (n1, n2) = t2_of_v128_arg_list f64 n in
        Concrete_v128.of_f64x2 n1 n2
      }
  | V128_CONST; I32X4; n = list(NUM); {
        let (n1, n2, n3, n4) = t4_of_v128_arg_list i32 n in
        Concrete_v128.of_i32x4 n1 n2 n3 n4
      }
  | V128_CONST; I64X2; n = list(NUM); {
        let (n1, n2) = t2_of_v128_arg_list i64 n in
        Concrete_v128.of_i64x2 n1 n2
      }

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
  | BR_ON_NULL; ~ = indice; <Br_on_null>
  | BR_ON_NON_NULL; ~ = indice; <Br_on_non_null>
  | RETURN; { Return }
  | RETURN_CALL; ~ = indice; <Return_call>
  | RETURN_CALL_REF; ~ = indice; { Return_call_ref (Bt_ind indice) }
  | CALL; ~ = indice; <Call>
  | CALL_REF; ~ = indice; { Call_ref (indice) }
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
  | n = v128_const; { V128_const n }
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
  (* simd *)
  | I32X4_ADD; { V_ibinop (I32x4, Add) }
  | I64X2_ADD; { V_ibinop (I64x2, Add) }
  | I32X4_SUB; { V_ibinop (I32x4, Sub) }
  | I64X2_SUB; { V_ibinop (I64x2, Sub) }
  (* ref *)
  | REF_NULL; ~ = heap_type; <Ref_null>
  | REF_IS_NULL; { Ref_is_null }
  | REF_AS_NON_NULL; { Ref_as_non_null }
  | REF_FUNC; ~ = indice; <Ref_func>
  (* i32 *)
  | I32_LOAD; id = memidx; memarg = memarg; { I_load (id, S32, memarg) }
  | I64_LOAD; id = memidx; memarg = memarg; { I_load (id, S64, memarg) }
  | F32_LOAD; id = memidx; memarg = memarg; { F_load (id, S32, memarg) }
  | F64_LOAD; id = memidx; memarg = memarg; { F_load (id, S64, memarg) }
  | I32_STORE; id = memidx; memarg = memarg; { I_store (id, S32, memarg) }
  | I64_STORE; id = memidx; memarg = memarg; { I_store (id, S64, memarg) }
  | F32_STORE; id = memidx; memarg = memarg; { F_store (id, S32, memarg) }
  | F64_STORE; id = memidx; memarg = memarg; { F_store (id, S64, memarg) }
  | I32_LOAD8_S; id = memidx; memarg = memarg; { I_load8 (id, S32, S, memarg ) }
  | I32_LOAD8_U; id = memidx; memarg = memarg; { I_load8 (id, S32, U, memarg ) }
  | I64_LOAD8_S; id = memidx; memarg = memarg; { I_load8 (id, S64, S, memarg ) }
  | I64_LOAD8_U; id = memidx; memarg = memarg; { I_load8 (id, S64, U, memarg ) }
  | I32_LOAD16_S; id = memidx; memarg = memarg; { I_load16 (id, S32, S, memarg )  }
  | I32_LOAD16_U; id = memidx; memarg = memarg; { I_load16 (id, S32, U, memarg ) }
  | I64_LOAD16_S; id = memidx; memarg = memarg; { I_load16 (id, S64, S, memarg ) }
  | I64_LOAD16_U; id = memidx; memarg = memarg; { I_load16 (id, S64, U, memarg ) }
  | I64_LOAD32_S; id = memidx; memarg = memarg; { I64_load32 (id, S, memarg) }
  | I64_LOAD32_U; id = memidx; memarg = memarg; { I64_load32 (id, U, memarg) }
  | I32_STORE8; id = memidx; memarg = memarg; { I_store8 (id, S32, memarg) }
  | I64_STORE8; id = memidx; memarg = memarg; { I_store8 (id, S64, memarg) }
  | I32_STORE16; id = memidx; memarg = memarg; { I_store16 (id, S32, memarg) }
  | I64_STORE16; id = memidx; memarg = memarg; { I_store16 (id, S64, memarg) }
  | I64_STORE32; id = memidx; memarg = memarg; { I64_store32 (id, memarg) }
  | MEMORY_SIZE; id = memidx; { Memory_size id }
  | MEMORY_GROW; id = memidx; { Memory_grow id }
  | MEMORY_FILL; id = memidx; { Memory_fill id }
  | MEMORY_COPY; {
    Memory_copy (Raw 0, Raw 0)
  }
  | MEMORY_COPY; src = indice; dst = indice; { Memory_copy (src, dst) }
  | MEMORY_INIT; args = list(indice); {
    match args with
    | [] -> failwith "memory.init: expected at least one argument"
    | [dataidx] -> Memory_init (Raw 0, dataidx)
    | memidx :: dataidx :: _ -> Memory_init (memidx, dataidx)
  }
  | DATA_DROP; ~ = indice; <Data_drop>
  (* extern *)
  | EXTERN_EXTERNALIZE; { Extern_externalize }
  | EXTERN_INTERNALIZE; { Extern_internalize }

(* Instructions & Expressions *)

let select_instr_instr_list ==
  | SELECT; (b, ts, es) = select_instr_results_instr_list; {
    Select (if b then (Some ts) else None) :: es
  }

let select_instr_results_instr_list :=
  | LPAR; RESULT; l = list(val_type); RPAR; (_, ts, es) = select_instr_results_instr_list; {
    true, l @ ts, es
  }
  | ~ = instr_list; {
    false, [], instr_list
  }

let call_indirect_prim ==
  | CALL_INDIRECT; { fun (id, typ) -> Call_indirect (id, typ) }
  | RETURN_CALL_INDIRECT; { fun (id, typ) -> Return_call_indirect (id, typ) }

let call_instr_instr_list ==
  | ~ = call_indirect_prim; ~ = indice; (x, es) = call_instr_type_instr_list; {
    call_indirect_prim (indice, x) :: es
  }
  | ~ = call_indirect_prim; (x, es) = call_instr_type_instr_list; {
    call_indirect_prim (Raw 0, x) :: es
  }

let call_instr_type_instr_list ==
  | ~ = type_use; ~ = call_instr_params_instr_list; {
    match call_instr_params_instr_list with
    | ([], []), es -> Bt_ind (type_use), es
    | (pt, rt), es -> Bt_raw ((Some type_use), ((List.map (fun t -> None, t) pt), rt)), es
  }
  | ((pt, rt), es) = call_instr_params_instr_list; { Bt_raw (None, (List.map (fun t -> None, t) pt, rt)), es }

let call_instr_params_instr_list :=
  | LPAR; PARAM; l = list(val_type); RPAR; ((ts1, ts2), es) = call_instr_params_instr_list; {
    (l @ ts1, ts2), es
  }
  | (ts, es) = call_instr_results_instr_list; {
    ([], ts), es
  }

let call_instr_results_instr_list :=
  | LPAR; RESULT; l = list(val_type); RPAR; (ts, es) = call_instr_results_instr_list; {
    l @ ts, es
  }
  | ~ = instr_list; {
    [], instr_list
  }

let block_instr ==
  | BLOCK; id = option(id); (bt, es) = block; END; id2 = option(id); {
    if Option.is_some id2 && not @@ Option.equal String.equal id id2
    then failwith "mismatching label";
    Block (id, bt, es)
  }
  | LOOP; id = option(id); (bt, es) = block; END; id2 = option(id); {
    if Option.is_some id2 && not @@ Option.equal String.equal id id2
    then failwith "mismatching label";
    Loop (id, bt, es)
  }
  | IF; id = option(id); (bt, es) = block; END; id2 = option(id); {
    if Option.is_some id2 && not @@ Option.equal String.equal id id2
    then failwith "mismatching label";
    If_else (id, bt, es, Annotated.dummy [])
  }
  | IF; id = option(id); (bt, es1) = block; ELSE; id2 = option(id); ~ = instr_list; END; id3 = option(id); {
    if (Option.is_some id2 && not @@ Option.equal String.equal id id2)
    || (Option.is_some id3 && not @@ Option.equal String.equal id id3)
    then failwith "mismatching label";
    let instr_list = Annotated.dummy_deep instr_list in
    If_else (id, bt, es1, instr_list)
  }

let block ==
  | ~ = type_use; (l, r) = block_param_body; {
    let block_type = match l with
    | ([], []) -> Bt_ind type_use
    | (pt, rt) -> Bt_raw ((Some type_use), (List.map (fun t -> None, t) pt, rt))
    in
    let r = Annotated.dummy_deep r in
    Some block_type, r
  }
  | (l, r) = block_param_body; {
    let block_type = match l with
      | [], [] -> None
      | (pt, rt) -> Some (Bt_raw (None, (List.map (fun t -> None, t) pt, rt)))
    in
    let r = Annotated.dummy_deep r in
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
  | ~ = plain_instr; ~ = expr_list; {
    let expr_list = Annotated.dummy expr_list in
    expr_list, plain_instr
  }
  | SELECT; (b, ts, es) = select_expr_result; {
    let es = Annotated.dummy es in
    es, Select (if b then Some ts else None)
  }
  | ~ = call_indirect_prim; ~ = indice; (x, es) = call_expr_type; {
    es, call_indirect_prim (indice, x)
  }
  | ~ = call_indirect_prim; (x, es) = call_expr_type; {
    es, call_indirect_prim (Raw 0, x)
  }
  | BLOCK; id = option(id); (bt, es) = block; {
    let l = Annotated.dummy [] in
    l, Block (id, bt, es)
  }
  | LOOP; id = option(id); (bt, es) = block; {
    let l = Annotated.dummy [] in
    l, Loop (id, bt, es)
  }
  | IF; id = option(id); (bt, ((pt, rt), (es, es1, es2))) = if_block; {
    let bt = match pt, rt with
    | [], [] -> bt
    | pt, rt ->
      let pt = List.map (fun t -> None, t) pt in
      let raw = pt, rt in
      begin match bt with
      | Some (Bt_ind type_use) -> Some (Bt_raw ((Some type_use), raw))
      | Some (Bt_raw _) -> failwith "unexpected Bt_raw"
      | None -> Some (Bt_raw (None, raw))
      end
    in
    let es1 = Annotated.dummy_deep es1 in
    let es2 = Annotated.dummy_deep es2.raw in
    es, If_else (id, bt, es1, es2)
  }

let expr :=
  | (es, e) = par(expr_aux); {
    let expr = es.raw @ [e] in
    expr
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
    | (pt, rt), es -> Bt_raw ((Some type_use), (List.map (fun t -> None, t) pt, rt)), es
  }
  | ((pt, rt), es) = call_expr_params; {
    Bt_raw (None, (List.map (fun t -> None, t) pt, rt)), es
  }

let call_expr_params :=
  | LPAR; PARAM; l = list(val_type); RPAR; ((ts1, ts2), es) = call_expr_params; {
    (l @ ts1, ts2), es
  }
  | (ts, es) = call_expr_results; {
    let es = Annotated.dummy es in
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
    let es0 = Annotated.dummy (expr @ es0.raw) in
    es0, es1, es2
  }
  | LPAR; THEN; es1 = instr_list; RPAR; LPAR; ELSE; es2 = instr_list; RPAR; {
    let es0 = Annotated.dummy [] in
    let es2 = Annotated.dummy es2 in
    es0, es1, es2
  }
  | LPAR; THEN; ~ = instr_list; RPAR; {
    let es0 = Annotated.dummy [] in
    let es1 = instr_list in
    let es2 = Annotated.dummy [] in
    es0, es1, es2
  }

let instr_list :=
  | { [] }
  | l = select_instr_instr_list; { l }
  | l = call_instr_instr_list; { l }
  | ~ = instr; ~ = instr_list; {
    instr @ instr_list
  }

let expr_list :=
  | { [] }
  | ~ = expr; ~ = expr_list; {
    let l = expr @ expr_list in
    l
  }

let const_expr ==
  | l = instr_list; {
    l
  }

(* Functions *)

let func ==
  | FUNC; id = option(id); ~ = func_fields; {
    let func_id = Option.map (fun id -> Text id) id in
    List.rev_map (function
      | Module.Field.Import i ->
        begin match i.typ with
        | Func (_id, ft) -> Module.Field.Import { i with typ = Func (id, ft) }
        | Global _ | Mem _ | Table _ | Tag _ -> assert false
        end
      | Export e -> Export { e with typ = Func func_id }
      | Func f -> Func { f with id }
      | Data _ | Tag _ | Elem _ | Global _ | Start _ | Typedef _ | Table _ | Mem _ as field -> begin
        Fmt.epr "got invalid field: `%a`@." Module.Field.pp field;
        assert false
      end
    ) func_fields
  }

let func_fields :=
  | ~ = type_use; (ft, f) = func_fields_body; {
    match ft with
    | [], [] -> [Func { f with type_f = Bt_ind type_use }]
    | (_pt, _rt) as ft -> [Func { f with type_f = Bt_raw ((Some type_use), ft)}]
  }
  | (type_f, f) = func_fields_body; {
    [Func { f with type_f = Bt_raw (None, type_f) }]
  }
  | (modul_name, name) = inline_import; ~ = type_use; _ = func_fields_import; {
    [Import { modul_name; name; typ = Func (None, Bt_ind type_use) }]
  }
  | (modul_name, name) = inline_import; ~ = func_fields_import; {
    [Import { modul_name; name; typ = Func (None, Bt_raw (None, func_fields_import)) }]
  }
  | ~ = inline_export; ~ = func_fields; {
    Module.Field.Export { name = inline_export; typ = Func None } :: func_fields
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
    let body = Annotated.dummy_deep body in
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
  | FUNC; { Null, Func_ht }

let elem_expr ==
  | LPAR; ITEM; l = const_expr; RPAR; {
    l
  }
  | l = expr; {
    l
  }

let elem_var ==
  | id = indice; {
    Annotated.dummies [Ref_func id]
  }

let elem_list ==
  | ~ = elem_kind; l = list(elem_var); {
    elem_kind, (l : expr list), false
  }
  | ~ = ref_type; l = list(elem_expr); {
    let l : expr list = List.map (List.map Annotated.dummy) l in
    ref_type, l, true
  }

let elem ==
  | ELEM; id = option(id); (typ, init, explicit_typ) = elem_list; {
    let init = Annotated.dummies init in
    [ Elem { id; typ; init; mode = Passive; explicit_typ } ]
  }
  | ELEM; id = option(id); table_use = par(table_use); ~ = offset; (typ, init, explicit_typ) = elem_list; {
    let init = Annotated.dummies init in
    let offset = Annotated.dummy_deep offset in
    [ Elem { id; typ; init; mode = Active (Some table_use, offset); explicit_typ } ]
  }
  | ELEM; id = option(id); DECLARE; (typ, init, explicit_typ) = elem_list; {
    let init = Annotated.dummies init in
    [ Elem { id; typ; init; mode = Declarative; explicit_typ } ]
  }
  | ELEM; id = option(id); ~ = offset; (typ, init, explicit_typ) = elem_list; {
    let init = Annotated.dummies init in
    let offset = Annotated.dummy_deep offset in
    [ Elem { id; typ; init; mode = Active (Some (Raw 0), offset); explicit_typ } ]
  }
  | ELEM; id = option(id); ~ = offset; init = list(elem_var); {
    let init =  Annotated.dummies init in
    let offset = Annotated.dummy_deep offset in
    [ Elem { id; typ = (Null, Func_ht); init; mode = Active (Some (Raw 0), offset); explicit_typ = false } ]
  }

let table ==
| TABLE; id = option(id); ~ = table_fields; {
  let tbl_id = Option.map (fun id -> Text id) id in
  List.rev_map (function
    | Module.Field.Table { id = _; typ; init } -> Module.Field.Table { id; typ; init }
    | Export e -> Export { e with typ = Table tbl_id }
    | Elem e ->
      let mode = Elem.Mode.Active (tbl_id, Annotated.dummy_deep [ I32_const 0l ]) in
      Elem { e with mode }
    | Import i -> begin match i.typ with
      | Table (_id, table_type) -> Import { i with typ = Table (id, table_type) }
      | Func _ | Global _ | Mem _ | Tag _ -> assert false
    end
    | Mem _ | Data _ | Tag _ | Start _ | Func _ | Global _ | Typedef _ as field -> begin
      Fmt.epr "got invalid field: `%a`@." Module.Field.pp field;
      assert false
    end
  ) table_fields
}

let init ==
  | l = list(elem_var); { List.map Annotated.dummy l }
  | l = nonempty_list(elem_expr); { List.map Annotated.dummy_deep l }

let table_fields :=
  | ~ = table_type; {
    [ Module.Field.Table { id = None; typ = table_type; init = None } ]
  }
  | ~ = table_type; init = expr; {
    [ Table { id = None; typ = table_type;
      init = Some (Annotated.dummy (List.map Annotated.dummy init)) } ]
  }
  | (modul_name, name) = inline_import; ~ = table_type; {
    [ Import { modul_name; name; typ = Table (None, table_type) }]
  }
  | ~ = inline_export; ~ = table_fields; {
    Export { name = inline_export; typ = Table None } :: table_fields
  }
  | ~ = ref_type; LPAR; ELEM; ~ = init; RPAR; {
    let min = List.fold_left (fun sum l ->
        sum + List.length l.Annotated.raw
      ) 0 init
    in
    let mode = Elem.Mode.Active (None, Annotated.dummy []) in
    [ Module.Field.Elem { id = None; typ = ref_type; init; mode; explicit_typ = true }
    ; Table  { id = None; typ = ({ min; max = Some min }, ref_type); init = None } ]
  }

let data ==
  | DATA; id = option(id); init = string_list; {
    [ Data { id; init; mode = Passive } ]
  }
  | DATA; id = option(id); memory_use = ioption(par(memory_use)); ~ = offset; init = string_list; {
    let memory_use = Option.value memory_use ~default:(Raw 0) in
    let offset = Annotated.dummy_deep offset in
    [ Data { id; init; mode = Active (Some memory_use, offset) } ]
  }

let memory ==
  | MEMORY; id = option(id); ~ = memory_fields; {
    let mem_id = Option.map (fun id -> Text id) id in
    List.rev_map (function
      | Module.Field.Mem (_id, m) -> Module.Field.Mem (id, m)
      | Export e -> Export { e with typ = Mem mem_id }
      | Data d ->
        let mode = Data.Mode.Active (mem_id, Annotated.dummy_deep [ I32_const 0l ]) in
        Data { d with mode }
      | Import i -> begin match i.typ with
        | Mem (_id, mem_type ) -> Import { i with typ = Mem (id, mem_type) }
        | Table _ | Func _ | Global _ | Tag _ -> assert false
        end
      | Tag _ | Elem _ | Typedef _ | Table _ | Func _ | Global _ | Start _ as field -> begin
        Fmt.epr "got invalid field: `%a`@." Module.Field.pp field;
        assert false
      end
    ) memory_fields
  }

let memory_fields :=
  | ~ = mem_type; {
    [ Mem (None, mem_type) ]
  }
  | (modul_name, name) = inline_import; ~ = mem_type; {
    [ Import { modul_name; name; typ = Mem (None, mem_type) } ]
  }
  | ~ = inline_export; ~ = memory_fields; {
    Export { name = inline_export; typ = Mem None } :: memory_fields
  }
  | LPAR; DATA; init = string_list; RPAR; {
    let min = Int32.(div (add (of_int (String.length init)) 65535l) 65536l) in
    let min = Int32.to_int min in
    [ Module.Field.Data { id = None; init; mode = Data.Mode.Active (None, Annotated.dummy []) }
    ; Mem (None, { min; max = Some min}) ]
  }

let global ==
  | GLOBAL; id = option(id); ~ = global_fields; {
    let global_id = Option.map (fun id -> Text id) id in
    List.rev_map (function
      | Module.Field.Global g -> Module.Field.Global { g with id }
      | Export e -> Export { e with typ = Global global_id }
      | Import i ->
        begin match i.typ with
        | Global (_id, t) -> Import { i with typ = Global (id, t) }
        | Mem _ | Table _ | Func _ | Tag _ -> assert false
        end
      | Start _ | Func _ | Data _ | Tag _ | Elem _ | Mem _ | Table _ | Typedef _ as field -> begin
        Fmt.epr "got invalid field: `%a`@." Module.Field.pp field;
        assert false
      end
    ) global_fields
  }

let global_fields :=
  | typ = global_type; init = const_expr; {
    let init : expr Annotated.t = Annotated.dummy_deep init in
    [ Module.Field.Global { typ; init; id = None } ]
  }
  | (modul_name, name) = inline_import; ~ = global_type; {
    [ Import { modul_name; name; typ = Global (None, global_type) } ]
  }
  | ~ = inline_export; ~ = global_fields; {
    Module.Field.Export { name = inline_export; typ = Global None } :: global_fields
  }

(* Imports & Exports *)

let import_type ==
  | FUNC; id = option(id); ~ = type_use; { Import.Type.Func (id, Bt_ind type_use) }
  | (id, ft) = preceded(FUNC, pair(option(id), func_type)); { Import.Type.Func (id, Bt_raw (None, ft)) }
  | TABLE; ~ = option(id); ~ = table_type; <Import.Type.Table>
  | MEMORY; ~ = option(id); ~ = mem_type; <Import.Type.Mem>
  | GLOBAL; ~ = option(id); ~ = global_type; <Import.Type.Global>
  | TAG; id = option(id); ~ = type_use; { Import.Type.Tag (id, Bt_ind type_use) }
  | (id, ft) = preceded(TAG, pair(option(id), func_type)); { Import.Type.Tag (id, Bt_raw (None, ft)) }

let import ==
  | IMPORT; modul_name = utf8_name; name = utf8_name; typ = par(import_type); {
    [ Module.Field.Import { modul_name; name; typ } ]
  }

let inline_import ==
  | LPAR; IMPORT; ln = utf8_name; rn = utf8_name; RPAR; { ln, rn }

let export_typ ==
  | FUNC; ~ = indice; { Export.Type.Func (Some indice) }
  | TABLE; ~ = indice; { Export.Type.Table (Some indice) }
  | MEMORY; ~ = indice; { Export.Type.Mem (Some indice) }
  | GLOBAL; ~ = indice; { Export.Type.Global (Some indice) }
  | TAG; ~ = indice; { Export.Type.Tag (Some indice) }

let export ==
  | EXPORT; name = utf8_name; typ = par(export_typ); {
    [ Export { name; typ; } ]
  }

let inline_export ==
  | LPAR; EXPORT; ~ = utf8_name; RPAR;  <>

let tag ==
  | TAG; id = option(id); fields = tag_fields; {
    let tag_id = Option.map (fun id -> Text id) id in
    List.rev_map (function
      | Module.Field.Tag t -> Module.Field.Tag { t with id }
      | Export e -> Export { e with typ = Tag tag_id }
      | Import i ->
        begin match i.typ with
        | Tag (_id, typ) -> Import { i with typ = Tag (id, typ) }
        | Mem _ | Table _ | Func _ | Global _ -> assert false
        end
      | Typedef _ | Global _ | Table _ | Mem _ | Func _ | Elem _  -> assert false
      | Data _ | Start _ -> assert false
    ) fields
  }

let tag_fields :=
  | ~ = type_use; {
    [ Module.Field.Tag { id = None; typ = Bt_ind type_use } ]
  }
  | ~ = tag_type; {
    [ Module.Field.Tag { id = None; typ = Bt_raw (None, tag_type) } ]
  }
  | (modul_name, name) = inline_import; ~ = tag_type; {
     [ Import { modul_name; name; typ = Tag (None, Bt_raw (None, tag_type)) }]
  }
  | ~ = inline_export; fields = tag_fields; {
    Module.Field.Export { name = inline_export; typ = Tag None } :: fields
  }

let tag_type :=
  | LPAR; PARAM; l = list(val_type); RPAR; (ins, out) = tag_type; {
    ((List.map (fun i -> None, i) l) @ ins, out)
  }
  | LPAR; PARAM; ~ = id; ~ = val_type; RPAR; (ins, out) = tag_type; {
    ((Some id, val_type) :: ins, out)
  }
  | { [], [] }

(* Modules *)

let type_field ==
  | ~ = type_def; { [ Module.Field.Typedef type_def ] }

let start ==
  | START; ~ = indice; { [ Start indice ] }

let module_field :=
  | ~ = type_field; <>
  | ~ = start; <>
  | ~ = export; <>
  | ~ = import; <>
  | ~ = elem; <>
  | ~ = data; <>
  | ~ = func; <>
  | ~ = global; <>
  | ~ = table; <>
  | ~ = memory; <>
  | ~ = tag; <>

let inline_module_inner ==
  | fields = list(par(module_field)); {
    let fields = List.flatten fields in
    let id = None in
    { Text.Module.id; fields; }
  }

let inline_module :=
  | inline_module = inline_module_inner; EOF; {
    inline_module
  }

let modul_nopar :=
  | id = ioption(id); ~ = inline_module_inner; {
    { inline_module_inner with id }
  }

let modul :=
  | LPAR; MODULE; m=modul_nopar; RPAR; { m }

let module_binary :=
  | id = ioption(id); BINARY; lines = list(NAME); {
    id, String.concat "" lines
  }

let module_quoted :=
  | QUOTE; lines = list(NAME); {
    String.concat "" lines
  }

let literal_const ==
  | I32_CONST; num = NUM; { Const_I32 (i32 num) }
  | I64_CONST; num = NUM; { Const_I64 (i64 num) }
  | F32_CONST; num = NUM; { Const_F32 (f32 num) }
  | F64_CONST; num = NUM; { Const_F64 (f64 num) }
  | n = v128_const; { Const_V128 n }
  | REF_NULL; ~ = heap_type; <Const_null>
  | REF_EXTERN; num = NUM; {
    match int_of_string_opt num with
    | None -> assert false
    | Some num -> Const_extern num
  }
  | REF_HOST; num = NUM; {
    match int_of_string_opt num with
    | None -> assert false
    | Some num -> Const_host num
  }

let const ==
  | ~ = literal_const; <Literal>
  | F32_CONST; NAN_CANON; { Nan_canon S32 }
  | F64_CONST; NAN_CANON; { Nan_canon S64 }
  | F32_CONST; NAN_ARITH; { Nan_arith S32 }
  | F64_CONST; NAN_ARITH; { Nan_arith S64 }

let result ==
  | ~ = const; <Result_const>
  | REF_EXTERN; { Result_extern_ref }
  | REF_FUNC; { Result_func_ref }
  | REF_NULL; { Result_const (Literal (Const_null Func_ht)) }

let assert_ ==
  | ASSERT_RETURN; ~ = par(action); ~ = list(par(result)); <Assert_return>
  | ASSERT_EXHAUSTION; ~ = par(action); ~ = NAME; <Assert_exhaustion>
  | ASSERT_TRAP; ~ = par(action); ~ = NAME; <Assert_trap>
  | ASSERT_TRAP; ~ = modul; ~ = NAME; <Assert_trap_module>
  | ASSERT_MALFORMED; ~ = modul; ~ = NAME; <Assert_malformed>
  | ASSERT_MALFORMED; LPAR; MODULE; QUOTE; lines = list(NAME); RPAR; expected = NAME; {
    let modul = String.concat "\n" lines in
    Assert_malformed_quote (modul, expected)
  }
  | ASSERT_MALFORMED; LPAR; MODULE; BINARY; lines = list(NAME); RPAR; expected = NAME; {
    let modul = String.concat "" lines in
    Assert_malformed_binary (modul, expected)
  }
  | ASSERT_INVALID; ~ = modul; ~ = NAME; <Assert_invalid>
  | ASSERT_INVALID; LPAR; MODULE; QUOTE; lines = list(NAME); RPAR; expected = NAME; {
    let modul = String.concat "\n" lines in
    Assert_invalid_quote (modul, expected)
  }
  | ASSERT_INVALID; LPAR; MODULE; BINARY; lines = list(NAME); RPAR; expected = NAME; {
    let modul = String.concat "" lines in
    Assert_invalid_binary (modul, expected)
  }
  | ASSERT_UNLINKABLE; ~ = modul; ~ = NAME; <Assert_unlinkable>

let register ==
  | REGISTER; ~ = utf8_name; ~ = option(id); <Register>

let action ==
  | INVOKE; ~ = ioption(id); ~ = utf8_name; ~ = list(par(literal_const)); <Invoke>
  | GET; ~ = ioption(id); ~ = utf8_name; <Get>

module_kind :
  | DEFINITION { true }
  | /* empty */ { false }

let cmd ==
  | LPAR; MODULE; kind = module_kind; m = modul_nopar; RPAR; {
    Text_module (kind, m)
  }
  | LPAR; MODULE; kind = module_kind; m = module_quoted; RPAR; {
    Quoted_module (kind, m)
  }
  | LPAR; MODULE; kind = module_kind; bm = module_binary; RPAR; {
    let id, m = bm in
    Binary_module (kind, id, m)
  }
  | LPAR; MODULE; INSTANCE; ~ = option(indice); RPAR; <Instance>
  | ~ = par(assert_); <Assert>
  | ~ = par(register); <>
  | ~ = par(action); <Action>

let script :=
  | ~ = nonempty_list(cmd); EOF; <>
  | ~ = inline_module_inner; EOF; {
    [ Text_module (false, inline_module_inner) ]
  }
