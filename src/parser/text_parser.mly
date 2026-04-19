%token <String.t> NUM
%token <String.t> ID NAME
%token ALIGN ANY_REF ARRAY ARRAY_FILL ARRAY_COPY ARRAY_GET ARRAY_GET_S ARRAY_GET_U ARRAY_LEN ARRAY_NEW_CANON ARRAY_NEW_CANON_DATA ARRAY_NEW_CANON_DEFAULT ARRAY_NEW_CANON_ELEM ARRAY_NEW_CANON_FIXED ARRAY_REF ARRAY_SET ARRAY_INIT_DATA ARRAY_INIT_ELEM ASSERT_EXHAUSTION ASSERT_INVALID ASSERT_MALFORMED ASSERT_RETURN ASSERT_TRAP ASSERT_UNLINKABLE ANY ANY_CONVERT_EXTERN EXTERN_CONVERT_ANY
%token BINARY BLOCK BR BR_IF BR_ON_CAST BR_ON_CAST_FAIL BR_ON_NON_NULL BR_ON_NULL BR_TABLE
%token CALL CALL_INDIRECT CALL_REF
%token DATA DATA_DROP DECLARE DROP
%token ELEM ELEM_DROP ELSE END EOF EQ EQ_REF EQUAL EXPORT EXTERN EXTERN_REF
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
%token TAG EXN NO_EXN EXN_REF NULL_EXN_REF REF_EXN
%token OFFSET
%token PARAM
%token QUOTE
%token REC REF REF_ARRAY REF_AS_NON_NULL REF_CAST REF_EQ REF_EXTERN REF_FUNC REF_HOST REF_I31 REF_IS_NULL REF_NULL REF_STRUCT REF_TEST REGISTER RESULT RETURN RETURN_CALL RETURN_CALL_INDIRECT RETURN_CALL_REF RPAR
%token SELECT START STRUCT STRUCT_GET STRUCT_GET_S STRUCT_GET_U STRUCT_NEW_CANON STRUCT_NEW_CANON_DEFAULT STRUCT_REF STRUCT_SET SUB
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
  if Int32.lt 0xFFl i || Int32.lt i (-0x80l) then
    Fmt.kstr failwith "constant out of range %s" s
  else Int32.to_int i

let i16 s =
  let i =
    try Int32.of_string_exn s
    with Failure msg -> Fmt.kstr failwith "constant out of range %s (%s)" s msg
  in
  if Int32.lt 0xFFFFl i || Int32.lt i (-0x8000l) then
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

let int_of_string n =
  match int_of_string_opt n with
  | None -> assert false
  | Some num -> num
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
  | NULL; { Null : nullable }
  | { No_null }

let heap_type ==
  | ANY; { Any_ht }
  | EQ; { Eq_ht }
  | I31; { I31_ht }
  | STRUCT; { Struct_ht }
  | ARRAY; { Array_ht }
  | NONE; { None_ht }
  | FUNC; { Func_ht }
  | NOFUNC; { NoFunc_ht }
  | EXN; { Exn_ht }
  | NO_EXN; { NoExn_ht }
  | EXTERN; { Extern_ht }
  | NOEXTERN; { NoExtern_ht }
  | ~ = indice; <TypeUse>

let ref_type ==
  | LPAR; REF; ~ = null_opt; ~ = heap_type; RPAR; <>
  | ANY_REF; { (Null : nullable), Any_ht }
  | EQ_REF; { (Null : nullable), Eq_ht }
  | I31_REF; { (Null : nullable), I31_ht }
  | STRUCT_REF; { (Null : nullable), Struct_ht }
  | ARRAY_REF; { (Null : nullable), Array_ht }
  | NULL_REF; { (Null : nullable), None_ht }
  | FUNC_REF; { (Null : nullable), Func_ht }
  | NULL_FUNC_REF; { (Null : nullable), NoFunc_ht }
  | EXN_REF; { (Null : nullable), Exn_ht }
  | NULL_EXN_REF; { (Null : nullable), NoExn_ht }
  | EXTERN_REF; { (Null : nullable), Extern_ht }
  | NULL_EXTERN_REF; { (Null : nullable), NoExtern_ht }

let packed_type :=
  | I8; { I8 }
  | I16; { I16 }

let val_type :=
  | ~ = num_type; <Num_type>
  | ~ = ref_type; <Ref_type>

let global_type :=
  | ~ = val_type; { (Const : mut), val_type }
  | val_type = par(preceded(MUTABLE, val_type)); { Var, val_type }

let storage_type ==
  | ~ = packed_type; <Pack_type>
  | ~ = val_type; <Val_type>

let field_type ==
  | st = storage_type; { (Const : mut), st }
  | LPAR; MUTABLE; t = storage_type; RPAR; { Var, t }

let struct_field :=
  | LPAR; FIELD; id = indice; ft = field_type; RPAR; { [(Some id, ft)] }
  | LPAR; FIELD; hdl = list(field_type); RPAR; {
    List.map (fun ft -> (None, ft)) hdl
   }

let func_type :=
  | o = list(par(preceded(RESULT, list(val_type)))); { [], List.flatten o }
  | i = par(preceded(PARAM, list(val_type))); (i2, o) = func_type; {
    (List.map (fun i -> None, i) i) @ i2, o
  }
  | LPAR; PARAM; ~ = id; ~ = val_type; RPAR; (i2, o) = func_type; {
    (Some id, val_type)::i2, o
  }

let comp_type :=
  | LPAR; STRUCT; fs = list(struct_field); RPAR; { Def_struct_t (List.flatten fs) }
  | LPAR; ARRAY; ft = field_type; RPAR; { Def_array_t ft }
  | LPAR; FUNC; ft = func_type; RPAR; { Def_func_t ft }

let sub_type :=
  | ct = comp_type;
    {{ final = true; ids = []; ct }}
  | LPAR; SUB; indices = list(indice); ct = comp_type; RPAR;
    {{ final = false; ids = indices; ct }}
  | LPAR; SUB; FINAL; indices = list(indice); ct = comp_type; RPAR;
    {{ final = true; ids = indices; ct }}

let simple_type_def :=
  // | TYPE; id = option(id); LPAR; FUNC; ~ = func_type; RPAR; { id, func_type }
  | TYPE; id = option(id); st = sub_type; { (id, st) }

let type_def :=
  | ty=simple_type_def; { SimpleType ty }
  | REC; tdl = list(par(simple_type_def)); {
    match tdl with
    | [] ->
      failwith "recursive type definitions expect at least one type definition"
    | [ st ] -> SimpleType st
    | _ -> RecType tdl
  }

let table_type ==
  | ~ = limits; ~ = ref_type; { limits, ref_type }

let mem_type ==
  | ~ = limits; <>

let addrtype ==
  | I64; { true }
  | I32; { false }
  | { false }

let limits ==
  | is_i64 = addrtype; min = NUM; {
    { is_i64; min; max = None}
  }
  | is_i64 = addrtype; min = NUM; max = NUM; {
    { is_i64; min; max = Some max }
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
  | I32; { Text.I32 : Text.num_type }
  | I64; { Text.I64 : Text.num_type }
  | F32; { Text.F32 : Text.num_type }
  | F64; { Text.F64 : Text.num_type }
  | V128; { Text.V128 : Text.num_type }

let memarg_align ==
  | ALIGN; EQUAL; n = NUM; <>

let memarg_offset ==
  | OFFSET; EQUAL; ~ = NUM; <>

let memarg ==
  | offset = option(memarg_offset); align = option(memarg_align); {
    {offset; align}
  }

let instr ==
  | ~ = plain_instr; { [plain_instr] }
  | ~ = block_instr; { [ block_instr ] }
  | ~ = expr; { expr }

let v128_const :=
  | V128_CONST; I8X16; n = list(NUM); {
    let (n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12, n13, n14, n15, n16) =
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
  | LOCAL_GET; ~ = indice; { Local (Get indice)}
  | LOCAL_SET; ~ = indice; { Local (Set indice) }
  | LOCAL_TEE; ~ = indice; { Local (Tee indice) }
  | GLOBAL_GET; ~ = indice; { Global (Get indice) }
  | GLOBAL_SET; ~ = indice; { Global (Set indice) }
  | TABLE_GET; indice = option(indice); {
    Table (Get (Option.value indice ~default:(Raw 0)))
  }
  | TABLE_SET; indice = option(indice); {
    Table (Set (Option.value indice ~default:(Raw 0)))
  }
  | TABLE_SIZE; indice = option(indice); {
    Table (Size (Option.value indice ~default:(Raw 0)))
  }
  | TABLE_GROW; indice = option(indice); {
    Table (Grow (Option.value indice ~default:(Raw 0)))
  }
  | TABLE_FILL; indice = option(indice); {
    Table (Fill (Option.value indice ~default:(Raw 0)))
  }
  | TABLE_COPY; {
    Table (Copy (Raw 0, Raw 0))
  }
  | TABLE_COPY; src = indice; dst = indice; { Table (Copy (src, dst)) }
  | TABLE_INIT; t_indice = ioption(indice); ~ = indice; {
    Table (Init (Option.value t_indice ~default:(Raw 0), indice))
  }
  | ELEM_DROP; ~ = indice; { Elem (Drop indice) }
  | I32_CONST; n = NUM; { I32 (Const (i32 n)) }
  | I64_CONST; n = NUM; { I64 (Const (i64 n)) }
  | F32_CONST; n = NUM; { F32 (Const (f32 n)) }
  | F64_CONST; n = NUM; { F64 (Const (f64 n)) }
  | n = v128_const; { V128 (Const n) }
  | I32_CLZ; { I32 Clz }
  | I64_CLZ; { I64 Clz }
  | I32_CTZ; { I32 Ctz }
  | I64_CTZ; { I64 Ctz }
  | I32_POPCNT; { I32 Popcnt }
  | I64_POPCNT; { I64 Popcnt }
  | F32_ABS; { F32 Abs }
  | F64_ABS; { F64 Abs }
  | F32_NEG; { F32 Neg }
  | F64_NEG; { F64 Neg }
  | F32_SQRT; { F32 Sqrt }
  | F64_SQRT; { F64 Sqrt }
  | F32_CEIL; { F32 Ceil }
  | F64_CEIL; { F64 Ceil }
  | F32_FLOOR; { F32 Floor }
  | F64_FLOOR; { F64 Floor }
  | F32_TRUNC; { F32 Trunc }
  | F64_TRUNC; { F64 Trunc }
  | F32_NEAREST; { F32 Nearest }
  | F64_NEAREST; { F64 Nearest }
  | I32_ADD; { I32 Add }
  | I64_ADD; { I64 Add }
  | I32_SUB; { I32 Sub }
  | I64_SUB; { I64 Sub }
  | I32_MUL; { I32 Mul }
  | I64_MUL; { I64 Mul }
  | I32_DIV_S; { I32 (Div S) }
  | I64_DIV_S; { I64 (Div S) }
  | I32_DIV_U; { I32 (Div U) }
  | I64_DIV_U; { I64 (Div U) }
  | I32_REM_S; { I32 (Rem S) }
  | I64_REM_S; { I64 (Rem S) }
  | I32_REM_U; { I32 (Rem U) }
  | I64_REM_U; { I64 (Rem U) }
  | I32_AND; { I32 And }
  | I64_AND; { I64 And }
  | I32_OR; { I32 Or }
  | I64_OR; { I64 Or }
  | I32_XOR; { I32 Xor }
  | I64_XOR; { I64 Xor }
  | I32_SHL; { I32 Shl }
  | I64_SHL; { I64 Shl }
  | I32_SHR_S; { I32 (Shr S) }
  | I64_SHR_S; { I64 (Shr S) }
  | I32_SHR_U; { I32 (Shr U) }
  | I64_SHR_U; { I64 (Shr U) }
  | I32_ROTL; { I32 Rotl }
  | I64_ROTL; { I64 Rotl }
  | I32_ROTR; { I32 Rotr }
  | I64_ROTR; { I64 Rotr }
  | F32_ADD; { F32 Add }
  | F64_ADD; { F64 Add }
  | F32_SUB; { F32 Sub }
  | F64_SUB; { F64 Sub }
  | F32_MUL; { F32 Mul }
  | F64_MUL; { F64 Mul }
  | F32_DIV; { F32 Div }
  | F64_DIV; { F64 Div }
  | F32_MIN; { F32 Min }
  | F64_MIN; { F64 Min }
  | F32_MAX; { F32 Max }
  | F64_MAX; { F64 Max }
  | F32_COPYSIGN; { F32 Copysign }
  | F64_COPYSIGN; { F64 Copysign }
  | I32_EQZ; { I32 Eqz }
  | I64_EQZ; { I64 Eqz }
  | I32_EQ; { I32 Eq }
  | I64_EQ; { I64 Eq }
  | I32_NE; { I32 Ne }
  | I64_NE; { I64 Ne }
  | I32_LT_S; { I32 (Lt S) }
  | I64_LT_S; { I64 (Lt S) }
  | I32_LT_U; { I32 (Lt U) }
  | I64_LT_U; { I64 (Lt U) }
  | I32_GT_S; { I32 (Gt S) }
  | I64_GT_S; { I64 (Gt S) }
  | I32_GT_U; { I32 (Gt U) }
  | I64_GT_U; { I64 (Gt U) }
  | I32_LE_S; { I32 (Le S) }
  | I64_LE_S; { I64 (Le S) }
  | I32_LE_U; { I32 (Le U) }
  | I64_LE_U; { I64 (Le U) }
  | I32_GE_S; { I32 (Ge S) }
  | I64_GE_S; { I64 (Ge S) }
  | I32_GE_U; { I32 (Ge U) }
  | I64_GE_U; { I64 (Ge U) }
  | F32_EQ; { F32 Eq }
  | F64_EQ; { F64 Eq }
  | F32_NE; { F32 Ne }
  | F64_NE; { F64 Ne }
  | F32_LT; { F32 Lt }
  | F64_LT; { F64 Lt }
  | F32_GT; { F32 Gt }
  | F64_GT; { F64 Gt }
  | F32_LE; { F32 Le }
  | F64_LE; { F64 Le }
  | F32_GE; { F32 Ge }
  | F64_GE; { F64 Ge }
  | I32_EXTEND8_S; { I32 Extend8_s }
  | I64_EXTEND8_S; { I64 Extend8_s }
  | I32_EXTEND16_S; { I32 Extend16_s }
  | I64_EXTEND16_S; { I64 Extend16_s }
  | I64_EXTEND32_S; { I64 Extend32_s }
  | I32_WRAP_I64; { I32 Wrap_i64 }
  | I64_EXTEND_I32_S; { I64 (Extend_i32 S) }
  | I64_EXTEND_I32_U; { I64 (Extend_i32 U) }
  | I32_TRUNC_F32_S; { I32 (Trunc_f (S32, S)) }
  | I32_TRUNC_F32_U; { I32 (Trunc_f (S32, U)) }
  | I32_TRUNC_F64_S; { I32 (Trunc_f (S64, S)) }
  | I32_TRUNC_F64_U; { I32 (Trunc_f (S64, U)) }
  | I64_TRUNC_F32_S; { I64 (Trunc_f (S32, S)) }
  | I64_TRUNC_F32_U; { I64 (Trunc_f (S32, U)) }
  | I64_TRUNC_F64_S; { I64 (Trunc_f (S64, S)) }
  | I64_TRUNC_F64_U; { I64 (Trunc_f (S64, U)) }
  | I32_TRUNC_SAT_F32_S; { I32 (Trunc_sat_f (S32, S)) }
  | I32_TRUNC_SAT_F32_U; { I32 (Trunc_sat_f (S32, U)) }
  | I32_TRUNC_SAT_F64_S; { I32 (Trunc_sat_f (S64, S)) }
  | I32_TRUNC_SAT_F64_U; { I32 (Trunc_sat_f (S64, U)) }
  | I64_TRUNC_SAT_F32_S; { I64 (Trunc_sat_f (S32, S)) }
  | I64_TRUNC_SAT_F32_U; { I64 (Trunc_sat_f (S32, U)) }
  | I64_TRUNC_SAT_F64_S; { I64 (Trunc_sat_f (S64, S)) }
  | I64_TRUNC_SAT_F64_U; { I64 (Trunc_sat_f (S64, U)) }
  | F32_DEMOTE_F64; { F32 (Demote_f64) }
  | F64_PROMOTE_F32; { F64 (Promote_f32) }
  | F32_CONVERT_I32_S; { F32 (Convert_i (S32, S)) }
  | F32_CONVERT_I32_U; { F32 (Convert_i (S32, U)) }
  | F32_CONVERT_I64_S; { F32 (Convert_i (S64, S)) }
  | F32_CONVERT_I64_U; { F32 (Convert_i (S64, U)) }
  | F64_CONVERT_I32_S; { F64 (Convert_i (S32, S)) }
  | F64_CONVERT_I32_U; { F64 (Convert_i (S32, U)) }
  | F64_CONVERT_I64_S; { F64 (Convert_i (S64, S)) }
  | F64_CONVERT_I64_U; { F64 (Convert_i (S64, U)) }
  | I32_REINTERPRET_F32; { I32 (Reinterpret_f S32) }
  | I32_REINTERPRET_F64; { I32 (Reinterpret_f S64) }
  | I64_REINTERPRET_F32; { I64 (Reinterpret_f S32) }
  | I64_REINTERPRET_F64; { I64 (Reinterpret_f S64) }
  | F32_REINTERPRET_I32; { F32 (Reinterpret_i S32) }
  | F32_REINTERPRET_I64; { F32 (Reinterpret_i S64) }
  | F64_REINTERPRET_I32; { F64 (Reinterpret_i S32) }
  | F64_REINTERPRET_I64; { F64 (Reinterpret_i S64) }
  (* simd *)
  | I32X4_ADD; { I32x4 Add }
  | I64X2_ADD; { I64x2 Add }
  | I32X4_SUB; { I32x4 Sub }
  | I64X2_SUB; { I64x2 Sub }
  (* ref *)
  | REF_NULL; ~ = heap_type; { Ref (Null heap_type) }
  | REF_IS_NULL; { Ref (Is_null) }
  | REF_AS_NON_NULL; { Ref (As_non_null) }
  | REF_FUNC; ~ = indice; { Ref (Func indice) }
  | REF_EQ; { Ref Eq }
  | REF_TEST; ~ = ref_type; { Ref (Test (ref_type)) }
  | REF_CAST; ~ = ref_type; { Ref (Cast (ref_type)) }
  (* i32 *)
  | I32_LOAD; id = memidx; memarg = memarg; { I32 (Load (id, memarg)) }
  | I64_LOAD; id = memidx; memarg = memarg; { I64 (Load (id, memarg)) }
  | F32_LOAD; id = memidx; memarg = memarg; { F32 (Load (id, memarg)) }
  | F64_LOAD; id = memidx; memarg = memarg; { F64 (Load (id, memarg)) }
  | I32_STORE; id = memidx; memarg = memarg; { I32 (Store (id, memarg)) }
  | I64_STORE; id = memidx; memarg = memarg; { I64 (Store (id, memarg)) }
  | F32_STORE; id = memidx; memarg = memarg; { F32 (Store (id, memarg)) }
  | F64_STORE; id = memidx; memarg = memarg; { F64 (Store (id, memarg)) }
  | I32_LOAD8_S; id = memidx; memarg = memarg; { I32 (Load8 (id, S, memarg) ) }
  | I32_LOAD8_U; id = memidx; memarg = memarg; { I32 (Load8 (id, U, memarg) ) }
  | I64_LOAD8_S; id = memidx; memarg = memarg; { I64 (Load8 (id, S, memarg) ) }
  | I64_LOAD8_U; id = memidx; memarg = memarg; { I64 (Load8 (id, U, memarg) ) }
  | I32_LOAD16_S; id = memidx; memarg = memarg; { I32 (Load16 (id, S, memarg) )  }
  | I32_LOAD16_U; id = memidx; memarg = memarg; { I32 (Load16 (id, U, memarg) ) }
  | I64_LOAD16_S; id = memidx; memarg = memarg; { I64 (Load16 (id, S, memarg) ) }
  | I64_LOAD16_U; id = memidx; memarg = memarg; { I64 (Load16 (id, U, memarg) ) }
  | I64_LOAD32_S; id = memidx; memarg = memarg; { I64 (Load32 (id, S, memarg)) }
  | I64_LOAD32_U; id = memidx; memarg = memarg; { I64 (Load32 (id, U, memarg)) }
  | I32_STORE8; id = memidx; memarg = memarg; { I32 (Store8 (id, memarg)) }
  | I64_STORE8; id = memidx; memarg = memarg; { I64 (Store8 (id, memarg)) }
  | I32_STORE16; id = memidx; memarg = memarg; { I32 (Store16 (id, memarg)) }
  | I64_STORE16; id = memidx; memarg = memarg; { I64 (Store16 (id, memarg)) }
  | I64_STORE32; id = memidx; memarg = memarg; { I64 (Store32 (id, memarg)) }
  | MEMORY_SIZE; id = memidx; { Memory (Size id) }
  | MEMORY_GROW; id = memidx; { Memory (Grow id) }
  | MEMORY_FILL; id = memidx; { Memory (Fill id) }
  | MEMORY_COPY; {
    Memory (Copy (Raw 0, Raw 0))
  }
  | MEMORY_COPY; src = indice; dst = indice; { Memory (Copy (src, dst)) }
  | MEMORY_INIT; args = list(indice); {
    match args with
    | [] -> failwith "memory.init: expected at least one argument"
    | [dataidx] -> Memory (Init (Raw 0, dataidx))
    | memidx :: dataidx :: _ -> Memory (Init (memidx, dataidx))
  }
  | DATA_DROP; ~ = indice; { Data (Drop indice) }
  (* aggregate types *)
  (* i31 *)
  | REF_I31; { Ref_i31 }
  | I31_GET_S; { I31_get_s }
  | I31_GET_U; { I31_get_u }
  (* struct *)
  | STRUCT_NEW_CANON; ~ = indice; <Struct_new>
  | STRUCT_NEW_CANON_DEFAULT; ~ = indice; <Struct_new_default>
  | STRUCT_GET; x = indice; i = indice; <Struct_get>
  | STRUCT_GET_S; x = indice; i = indice; <Struct_get_s>
  | STRUCT_GET_U; x = indice; i = indice; <Struct_get_u>
  | STRUCT_SET; x = indice; i = indice; <Struct_set>
  (* array *)
  | ARRAY_NEW_CANON; ~ = indice; <Array_new>
  | ARRAY_NEW_CANON_DEFAULT; x = indice; <Array_new_default>
  | ARRAY_NEW_CANON_FIXED; x = indice; n = NUM; {Array_new_fixed (x, i32 n)}
  | ARRAY_NEW_CANON_DATA; x = indice; y = indice; <Array_new_data>
  | ARRAY_NEW_CANON_ELEM; x = indice; y = indice; <Array_new_elem>
  | ARRAY_GET; ~ = indice; <Array_get>
  | ARRAY_GET_S; ~ = indice; <Array_get_s>
  | ARRAY_GET_U; ~ = indice; <Array_get_u>
  | ARRAY_SET; ~ = indice; <Array_set>
  | ARRAY_LEN; { Array_len }
  | ARRAY_FILL; ~ = indice; <Array_fill>
  | ARRAY_COPY; x1 = indice; x2 = indice; <Array_copy>
  | ARRAY_INIT_DATA; x = indice; y = indice; <Array_init_data>
  | ARRAY_INIT_ELEM; x = indice; y = indice; <Array_init_elem>
  | ANY_CONVERT_EXTERN; { Any_convert_extern }
  | EXTERN_CONVERT_ANY; { Extern_convert_any }

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
    If_else (id, bt, es, [])
  }
  | IF; id = option(id); (bt, es1) = block; ELSE; id2 = option(id); ~ = instr_list; END; id3 = option(id); {
    if (Option.is_some id2 && not @@ Option.equal String.equal id id2)
    || (Option.is_some id3 && not @@ Option.equal String.equal id id3)
    then failwith "mismatching label";
    If_else (id, bt, es1, instr_list)
  }

let block ==
  | ~ = type_use; (l, r) = block_param_body; {
    let block_type = match l with
    | ([], []) -> Bt_ind type_use
    | (pt, rt) -> Bt_raw ((Some type_use), (List.map (fun t -> None, t) pt, rt))
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
  | ~ = plain_instr; ~ = expr_list; {
    expr_list, plain_instr
  }
  | SELECT; (b, ts, es) = select_expr_result; {
    es, Select (if b then Some ts else None)
  }
  | ~ = call_indirect_prim; ~ = indice; (x, es) = call_expr_type; {
    es, call_indirect_prim (indice, x)
  }
  | ~ = call_indirect_prim; (x, es) = call_expr_type; {
    es, call_indirect_prim (Raw 0, x)
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
      | Some (Bt_ind type_use) -> Some (Bt_raw ((Some type_use), raw))
      | Some (Bt_raw _) -> failwith "unexpected Bt_raw"
      | None -> Some (Bt_raw (None, raw))
      end
    in
    es, If_else (id, bt, es1, es2)
  }

let expr :=
  | (es, e) = par(expr_aux); {
    let expr = es @ [e] in
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
    let es0 = (expr @ es0) in
    es0, es1, es2
  }
  | LPAR; THEN; es1 = instr_list; RPAR; LPAR; ELSE; es2 = instr_list; RPAR; {
    let es0 = [] in
    es0, es1, es2
  }
  | LPAR; THEN; ~ = instr_list; RPAR; {
    let es0 = [] in
    let es1 = instr_list in
    let es2 = [] in
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
  | FUNC; { (Null : nullable), Func_ht }

let elem_expr ==
  | LPAR; ITEM; l = const_expr; RPAR; {
    l
  }
  | l = expr; {
    l
  }

let elem_var ==
  | id = indice; {
    [ Ref (Func id) ]
  }

let elem_list ==
  | ~ = elem_kind; l = list(elem_var); {
    elem_kind, (l : expr list), false
  }
  | ~ = ref_type; l = list(elem_expr); {
    ref_type, l, true
  }

let elem ==
  | ELEM; id = option(id); (typ, init, explicit_typ) = elem_list; {
    [ Elem { id; typ; init; mode = Passive; explicit_typ } ]
  }
  | ELEM; id = option(id); table_use = par(table_use); ~ = offset; (typ, init, explicit_typ) = elem_list; {
    [ Elem { id; typ; init; mode = Active (Some table_use, offset); explicit_typ } ]
  }
  | ELEM; id = option(id); DECLARE; (typ, init, explicit_typ) = elem_list; {
    [ Elem { id; typ; init; mode = Declarative; explicit_typ } ]
  }
  | ELEM; id = option(id); ~ = offset; (typ, init, explicit_typ) = elem_list; {
    [ Elem { id; typ; init; mode = Active (Some (Raw 0), offset); explicit_typ } ]
  }
  | ELEM; id = option(id); ~ = offset; init = list(elem_var); {
    [ Elem { id; typ = ((Null : nullable), Func_ht); init; mode = Active (Some (Raw 0), offset); explicit_typ = false } ]
  }

let table ==
| TABLE; id = option(id); ~ = table_fields; {
  let tbl_id = Option.map (fun id -> Text id) id in
  List.rev_map (function
    | Module.Field.Table { id = _; typ; init } -> Module.Field.Table { id; typ; init }
    | Export e -> Export { e with typ = Table tbl_id }
    | Elem e ->
      let mode = Elem.Mode.Active (tbl_id, [ I32 (Const 0l) ]) in
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
  | l = list(elem_var); { l }
  | l = nonempty_list(elem_expr); { l }

let table_fields :=
  | ~ = table_type; {
    [ Module.Field.Table { id = None; typ = table_type; init = None } ]
  }
  | ~ = table_type; init = expr; {
    [ Table { id = None; typ = table_type; init = Some (init) } ]
  }
  | (modul_name, name) = inline_import; ~ = table_type; {
    [ Import { modul_name; name; typ = Table (None, table_type) }]
  }
  | ~ = inline_export; ~ = table_fields; {
    Export { name = inline_export; typ = Table None } :: table_fields
  }
  | ~ = ref_type; LPAR; ELEM; ~ = init; RPAR; {
    let min = List.fold_left (fun sum l ->
        sum + (List.length l)
      ) 0 init
    in
    let mode = Elem.Mode.Active (None, []) in
    [ Module.Field.Elem { id = None; typ = ref_type; init; mode; explicit_typ = true }
    ; Table  { id = None; typ = ({ is_i64 = false; min = string_of_int min; max = Some (string_of_int min) }, ref_type); init = None } ]
  }

let data ==
  | DATA; id = option(id); init = string_list; {
    [ Data { id; init; mode = Passive } ]
  }
  | DATA; id = option(id); memory_use = ioption(par(memory_use)); ~ = offset; init = string_list; {
    let memory_use = Option.value memory_use ~default:(Raw 0) in
    [ Data { id; init; mode = Active (Some memory_use, offset) } ]
  }

let memory ==
  | MEMORY; id = option(id); ~ = memory_fields; {
    let mem_id = Option.map (fun id -> Text id) id in
    List.rev_map (function
      | Module.Field.Mem (_id, m) -> Module.Field.Mem (id, m)
      | Export e -> Export { e with typ = Mem mem_id }
      | Data d ->
        let mode = Data.Mode.Active (mem_id, [ I32 (Const 0l) ]) in
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
    let min = ((((String.length init)) + 65535) / 65536) in
    [ Module.Field.Data { id = None; init; mode = Data.Mode.Active (None, []) }
    ; Mem (None, { is_i64 = false; min = string_of_int min; max = Some (string_of_int min)}) ]
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
    let init : expr = init in
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
  | REF_NULL; ht = heap_type; { Const_null (Some ht)}
  | REF_STRUCT; num = NUM; { Const_struct (int_of_string num) }
  | REF_ARRAY; num = NUM; { Const_array (int_of_string num) }
  | REF_FUNC; num = NUM; { Const_func (int_of_string num) }
  | REF_EXN; num = NUM; { Const_exn (int_of_string num) }
  | REF_EXTERN; num = NUM; { Const_extern (int_of_string num) }
  | REF_HOST; num = NUM; { Const_host (int_of_string num) }

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
  | REF_NULL; { Result_const (Literal (Const_null None)) }
  | REF_ARRAY; { Result_array_ref }
  | REF_STRUCT; { Result_struct_ref }
  | REF_EQ; { Result_eq_ref }
  | REF_I31; { Result_i31_ref }

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

let instance ==
  | MODULE; INSTANCE; ~ = ioption(id); ~ = id; <Instance>

let register ==
  | REGISTER; ~ = utf8_name; ~ = option(id); <Register>

let action ==
  | INVOKE; ~ = ioption(id); ~ = utf8_name; ~ = list(par(literal_const)); <Invoke>
  | GET; indice = ioption(id); ~ = utf8_name; {
    Get (indice, utf8_name) : Wast.action
  }

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
  | ~ = par(assert_); <Assert>
  | ~ = par(instance); <>
  | ~ = par(register); <>
  | ~ = par(action); <Action>

let script :=
  | ~ = nonempty_list(cmd); EOF; <>
  | ~ = inline_module_inner; EOF; {
    [ Text_module (false, inline_module_inner) ]
  }
