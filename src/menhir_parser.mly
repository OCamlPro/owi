%token <String.t> NUM
%token<String.t> ID NAME
%token NAN_ARITH NAN_CANON ASSERT_INVALID ASSERT_MALFORMED ASSERT_EXHAUSTION ASSERT_TRAP ASSERT_UNLINKABLE PARAM RESULT FUNCREF EXTERNREF I32 I64 F32 F64 CLZ CTZ POPCNT ABS NEG SQRT CEIL FLOOR TRUNC NEAREST SIGNED UNSIGNED ADD SUB MUL DIV REM AND OR XOR SHL SHR ROTL ROTR MIN MAX COPYSIGN EQZ EQ NE LT GT LE GE EXTEND8 EXTEND16 EXTEND32 EXTEND_I32 WRAP_I64 TABLE GROW INIT COPY TEE ITEM REF SELECT DEMOTE_F64 DROP UNDERSCORE GET FILL CONVERT PROMOTE_F32 SIZE SET IS_NULL LOCAL NULL REINTERPRET GLOBAL ELEM STORE8 STORE16 STORE STORE32 BR_TABLE CALL LOAD LOAD8 LOAD16 LOOP DATA BR_IF BR OFFSET UNREACHABLE CALL_INDIRECT LOAD32 BLOCK ALIGN EQUAL MEMORY RETURN NOP FUNC EXPORT IMPORT EXTERN MUTABLE MODULE RPAR LPAR EOF IF ELSE THEN DOT CONST START TYPE DECLARE END INVOKE ASSERT_RETURN QUOTE REGISTER TRUNC_SAT BINARY

%{
let u32_of_i32 = Uint32.of_int32

let u32 s =
  try Uint32.of_string s
  with Failure _ -> failwith (Format.sprintf "error u32 constant `%s` out of range" s)

let i32 s = Int32.of_string s

let i64 s = Int64.of_string s

let f64 s = Float64.of_string s

let f32 s = Float32.of_string s

open Types

%}

%start <Types.file> file

%%

(* Helpers *)

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
  | min = NUM; { {min = i32 min; max = None} }
  | min = NUM; max = NUM; { {min = i32 min; max = Some (i32 max) } }

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

let inn ==
  | I32; { Types.S32 }
  | I64; { Types.S64 }

let fnn ==
  | F32; { Types.S32 }
  | F64; { Types.S64 }

let sx ==
  | SIGNED; { S }
  | UNSIGNED; { U }

let iunop ==
  | CLZ; { Clz }
  | CTZ; { Ctz }
  | POPCNT; { Popcnt }

let ibinop ==
  | ADD; { (Add: Types.ibinop) }
  | SUB; { (Sub: Types.ibinop) }
  | MUL; { (Mul: Types.ibinop) }
  | DIV; s = sx; { (Div s: Types.ibinop) }
  | REM; s = sx; { Rem s }
  | AND; { And }
  | OR; { Or }
  | XOR; { Xor }
  | SHL; { Shl }
  | SHR; s = sx; { Shr s }
  | ROTL; { Rotl }
  | ROTR; { Rotr }

let itestop ==
  | EQZ; { Eqz }

let irelop ==
  | EQ; { (Eq : Types.irelop) }
  | NE; { (Ne : Types.irelop) }
  | LT; s = sx; { (Lt s: Types.irelop) }
  | GT; s = sx; { (Gt s: Types.irelop) }
  | LE; s = sx; { (Le s: Types.irelop) }
  | GE; s = sx; { (Ge s: Types.irelop) }

let funop ==
  | ABS; { Abs }
  | NEG; { Neg }
  | SQRT; { Sqrt }
  | CEIL; { Ceil }
  | FLOOR; { Floor }
  | TRUNC; { Trunc }
  | NEAREST; { Nearest }

let fbinop ==
  | ADD; { Add }
  | SUB; { Sub }
  | MUL; { Mul }
  | DIV; { Div }
  | MIN; { Min }
  | MAX; { Max }
  | COPYSIGN; { Copysign }

let frelop ==
  | EQ; { Eq }
  | NE; { Ne }
  | LT; { Lt }
  | GT; { Gt }
  | LE; { Le }
  | GE; { Ge }

let align ==
  | ALIGN; EQUAL; ~ = NUM; <>

let memarg_offset ==
  | OFFSET; EQUAL; ~ = NUM; <>

let memarg ==
  | offset = option(memarg_offset); align = option(align); {
    (* TODO: check default *)
    let offset = u32 @@ Option.value offset ~default:"0" in
    let align = u32 @@ Option.value align ~default:"0" in
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
  | LOCAL; DOT; GET; ~ = indice; <Local_get>
  | LOCAL; DOT; SET; ~ = indice; <Local_set>
  | LOCAL; DOT; TEE; ~ = indice; <Local_tee>
  | GLOBAL; DOT; GET; ~ = indice; <Global_get>
  | GLOBAL; DOT; SET; ~ = indice; <Global_set>
  | TABLE; DOT; GET; indice = option(indice); {
    Table_get (Option.value indice ~default:(Raw (u32_of_i32 0l)))
  }
  | TABLE; DOT; SET; indice = option(indice); {
    Table_set (Option.value indice ~default:(Raw (u32_of_i32 0l)))
  }
  | TABLE; DOT; SIZE; indice = option(indice); {
    Table_size (Option.value indice ~default:(Raw (u32_of_i32 0l)))
  }
  | TABLE; DOT; GROW; indice = option(indice); {
    Table_grow (Option.value indice ~default:(Raw (u32_of_i32 0l)))
  }
  | TABLE; DOT; FILL; indice = option(indice); {
    Table_fill (Option.value indice ~default:(Raw (u32_of_i32 0l)))
  }
  | TABLE; DOT; COPY; {
    Table_copy (
      Raw (u32_of_i32 0l),
      Raw (u32_of_i32 0l)
    )
  }
  | TABLE; DOT; COPY; src = indice; dst = indice; { Table_copy (src, dst) }
  | TABLE; DOT; INIT; t_indice = ioption(indice); ~ = indice; {
    Table_init (Option.value t_indice ~default:(Raw (u32_of_i32 0l)), indice)
  }
  (* TODO: check they're actually plain_instr and not instr: *)
  (* TODO: check that nothing is missing *)
  | ELEM; DOT; DROP; ~ = indice; <Elem_drop>
  | I32; DOT; CONST; n = NUM; { I32_const (i32 n) }
  | I64; DOT; CONST; n = NUM; { I64_const (i64 n) }
  | F32; DOT; CONST; n = NUM; { F32_const (f32 n) }
  | F64; DOT; CONST; n = NUM; { F64_const (f64 n) }
  | ~ = inn; DOT; ~ = iunop; <I_unop>
  | ~ = fnn; DOT; ~ = funop; <F_unop>
  | ~ = inn; DOT; ~ = ibinop; <I_binop>
  | ~ = fnn; DOT; ~ = fbinop; <F_binop>
  | ~ = inn; DOT; ~ = itestop; <I_testop>
  | ~ = inn; DOT; ~ = irelop; <I_relop>
  | ~ = fnn; DOT; ~ = frelop; <F_relop>
  | ~ = inn; DOT; EXTEND8; SIGNED; <I_extend8_s>
  | ~ = inn; DOT; EXTEND16; SIGNED; <I_extend16_s>
  | I64; DOT; EXTEND32; SIGNED; { I64_extend32_s }
  | I32; DOT; WRAP_I64; { I32_wrap_i64 }
  | I64; DOT; EXTEND_I32; ~ = sx; <I64_extend_i32>
  | ~ = inn; DOT; TRUNC; UNDERSCORE; ~ = fnn; s = sx; <I_trunc_f>
  | ~ = inn; DOT; TRUNC_SAT; UNDERSCORE; ~ = fnn; s = sx; <I_trunc_sat_f>
  | F32; DOT; DEMOTE_F64; { F32_demote_f64 }
  | F64; DOT; PROMOTE_F32; { F64_promote_f32 }
  | ~ = fnn; DOT; CONVERT; UNDERSCORE; ~ = inn; s = sx; <F_convert_i>
  | ~ = inn; DOT; REINTERPRET; UNDERSCORE; ~ = fnn; <I_reinterpret_f>
  | ~ = fnn; DOT; REINTERPRET; UNDERSCORE; ~ = inn; <F_reinterpret_i>
  | REF; DOT; NULL; ~ = ref_kind; <Ref_null>
  | REF; DOT; IS_NULL; { Ref_is_null }
  | REF; DOT; FUNC; ~ = indice; <Ref_func>
  | ~ = inn; DOT; LOAD; ~ = memarg; <I_load>
  | ~ = fnn; DOT; LOAD; ~ = memarg; <F_load>
  | ~ = inn; DOT; STORE; ~ = memarg; <I_store>
  | ~ = fnn; DOT; STORE; ~ = memarg; <F_store>
  | ~ = inn; DOT; LOAD8; ~ = sx; ~ = memarg; <I_load8>
  | ~ = inn; DOT; LOAD16; ~ = sx; ~ = memarg; <I_load16>
  | I64; DOT; LOAD32; ~ = sx; ~ = memarg; <I64_load32>
  | ~ = inn; DOT; STORE8; ~ = memarg; <I_store8>
  | ~ = inn; DOT; STORE16; ~ = memarg; <I_store16>
  | I64; DOT; STORE32; ~ = memarg; <I64_store32>
  | MEMORY; DOT; SIZE; { Memory_size }
  | MEMORY; DOT; GROW; { Memory_grow }
  | MEMORY; DOT; FILL; { Memory_fill }
  | MEMORY; DOT; COPY; { Memory_copy }
  | MEMORY; DOT; INIT; ~ = indice; <Memory_init>
  | DATA; DOT; DROP; ~ = indice; <Data_drop>

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
    Call_indirect (Raw (u32_of_i32 0l), call_instr_type)
  }

let call_instr_type ==
  | ~ = type_use; ~ = call_instr_params; {
    match call_instr_params with
    | ([], []) -> Bt_ind (type_use)
    | (pt, rt) -> Bt_raw (List.map (fun t -> None, t) pt, rt) (* TODO: inline_type_explicit type_use ft *)
  }
  | (pt, rt) = call_instr_params; { Bt_raw (List.map (fun t -> None, t) pt, rt) }

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
    Call_indirect (Raw (u32_of_i32 0l), x), es
  }

let call_instr_type_instr ==
  | ~ = type_use; ~ = call_instr_params_instr; {
    match call_instr_params_instr with
    | ([], []), es -> Bt_ind (type_use), es
    | (pt, rt), es -> Bt_raw ((List.map (fun t -> None, t) pt), rt), es (* TODO: inline_type_explicit t ft, es *)
  }
  | ((pt, rt), es) = call_instr_params_instr; { Bt_raw (List.map (fun t -> None, t) pt, rt), es }

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
    if Option.is_some id && Option.is_some id2 then assert (id = id2);
    Block (id, bt, es)
  }
  | LOOP; id = option(id); (bt, es) = block; END; id2 = option(id); {
    if Option.is_some id && Option.is_some id2 then assert (id = id2);
    Loop (id, bt, es)
  }
  | IF; id = option(id); (bt, es) = block; END; id2 = option(id); {
    if Option.is_some id && Option.is_some id2 then assert (id = id2);
    If_else (id, bt, es, [])
  }
  | IF; id = option(id); (bt, es1) = block; ELSE; id2 = option(id); ~ = instr_list; END; id3 = option(id); {
    if Option.is_some id && Option.is_some id2 then assert (id = id2);
    if Option.is_some id && Option.is_some id3 then assert (id = id3);
    If_else (id, bt, es1, instr_list)
  }

let block ==
  | ~ = type_use; (l, r) = block_param_body; {
    let block_type = match l with
    | ([], []) -> Bt_ind type_use
    | (pt, rt) -> Bt_raw (List.map (fun t -> None, t) pt, rt) (* TODO: type_use ? *)
    in
    Some block_type, r
  }
  | (l, r) = block_param_body; {
    let block_type = match l with
      | [], [] -> None
      | (pt, rt) -> Some (Bt_raw (List.map (fun t -> None, t) pt, rt))
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
    es, Call_indirect (Raw (u32_of_i32 0l), x)
  }
  | BLOCK; id = option(id); (bt, es) = block; {
    [], Block (id, bt, es)
  }
  | LOOP; id = option(id); (bt, es) = block; {
    [], Loop (id, bt, es)
  }
  | IF; id = option(id); (bt, (_param_or_result, (es, es1, es2))) = if_block; {
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
    | (pt, rt), es -> Bt_raw (List.map (fun t -> None, t) pt, rt), es (* TODO: type_use ? *)
  }
  | ((pt, rt), es) = call_expr_params; {
    Bt_raw (List.map (fun t -> None, t) pt, rt), es
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
      | _field -> (Format.eprintf "got invalid field: `%a`@." Pp.module_field _field; assert false)
    ) func_fields
  }

let func_fields :=
  | ~ = type_use; (_todo, f) = func_fields_body; {
    [MFunc { f with type_f = Bt_ind type_use }]
  }
  | (type_f, f) = func_fields_body; {
    [MFunc { f with type_f = Bt_raw type_f }]
  }
  | (module_, name) = inline_import; ~ = type_use; _ = func_fields_import; {
    [MImport { module_; name; desc = Import_func (None, Bt_ind type_use) }]
  }
  | (module_, name) = inline_import; ~ = func_fields_import; {
    [MImport { module_; name; desc = Import_func (None, Bt_raw func_fields_import) }]
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
    { type_f = Bt_ind (Raw (u32_of_i32 (-1l))); locals = []; body; id = None }
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
    [ MElem { id; type_; init; mode = Elem_active (Some (Raw Unsigned.UInt32.zero), offset) } ]
  }
  | ELEM; id = option(id); ~ = offset; init = list(elem_var); {
    [ MElem { id; type_ = Func_ref; init = [init]; mode = Elem_active (Some (Raw Unsigned.UInt32.zero), offset) } ]
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
    | _field -> (Format.eprintf "got invalid field: `%a`@." Pp.module_field _field; assert false)
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
    let min = Int32.of_int @@ List.length init in
    [ MElem { id = None; type_ = Func_ref; init; mode = Elem_active (None, []) }
    ; MTable (None, ({ min; max = Some min }, ref_type)) ]
  }

let data ==
  | DATA; id = option(id); init = string_list; {
    [ MData { id; init; mode = Data_passive } ]
  }
  | DATA; id = option(id); memory_use = ioption(par(memory_use)); ~ = offset; init = string_list; {
    let memory_use = Option.value memory_use ~default:(Raw (u32_of_i32 0l)) in
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
      | _field -> (Format.eprintf "got invalid field: `%a`@." Pp.module_field _field; assert false)
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
      | _field -> (Format.eprintf "got invalid field: `%a`@." Pp.module_field _field; assert false)
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
  | (id, ft) = preceded(FUNC, pair(option(id), func_type)); { Import_func (id, Bt_raw ft) }
  | TABLE; ~ = option(id); ~ = table_type; <Import_table>
  | MEMORY; ~ = option(id); ~ = mem_type; <Import_mem>
  | GLOBAL; ~ = option(id); ~ = global_type; <Import_global>

let import ==
  | IMPORT; module_ = NAME; name = NAME; desc = par(import_desc); {
    [ MImport { module_; name; desc } ]
  }

let inline_import ==
  | LPAR; IMPORT; ~ = NAME; ~ = NAME; RPAR; <>

let export_desc ==
  | FUNC; ~ = indice; { Export_func (Some indice) }
  | TABLE; ~ = indice; { Export_table (Some indice) }
  | MEMORY; ~ = indice; { Export_mem (Some indice) }
  | GLOBAL; ~ = indice; { Export_global (Some indice) }

let export ==
  | EXPORT; name = NAME; desc = par(export_desc); {
    [ MExport { name; desc; } ]
  }

let inline_export ==
  | LPAR; EXPORT; ~ = NAME; RPAR;  <>

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
  | ~ = num_type; DOT; CONST; num = NUM; {
    match num_type with
    | I32 -> Const_I32 (i32 num)
    | I64 -> Const_I64 (i64 num)
    | F32 -> Const_F32 (f32 num)
    | F64 -> Const_F64 (f64 num)
  }
  | REF; DOT; NULL; ~ = ref_kind; <Const_null>
  | REF; DOT; EXTERN; num = NUM; { Const_host (int_of_string num) }

let const ==
  | ~ = literal_const; <Literal>
  | ~ = num_type; DOT; CONST; NAN_CANON; {
    match num_type with
    | F32 -> Nan_canon S32
    | F64 -> Nan_canon S64
    | I32 | I64 -> failwith "nan:canonical can't be an int"
  }
  | ~ = num_type; DOT; CONST; NAN_ARITH; {
    match num_type with
    | F32 -> Nan_arith S32
    | F64 -> Nan_arith S64
    | I32 | I64 -> failwith "nan:arithmetic can't be an int"
  }

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
  | REGISTER; ~ = NAME; ~ = option(id); <Register>

let action ==
  | INVOKE; ~ = ioption(id); ~ = NAME; ~ = list(par(literal_const)); <Invoke>
  | GET; ~ = ioption(id); ~ = NAME; <Get>

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
