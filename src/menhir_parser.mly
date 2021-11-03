%token<String.t> ID
%token<String.t> NAME
%token<Unsigned.UInt32.t> U32
%token<Int64.t> INT
%token<Float.t> FLOAT
%token PARAM RESULT
%token FUNC_REF EXTERN_REF
%token I32 I64 F32 F64
%token CLZ CTZ POPCNT
%token ABS NEG SQRT CEIL FLOOR TRUNC NEAREST
%token SIGNED UNSIGNED
%token ADD SUB MUL DIV REM AND OR XOR SHL SHR ROTL ROTR MIN MAX COPYSIGN
%token EQZ
%token EQ NE LT GT LE GE
%token EXTEND8 EXTEND16 EXTEND32 EXTEND_I32 WRAPI64
%token TABLE GROW INIT COPY
%token TEE
%token REF
%token SELECT
%token DEMOTE_F64
%token DROP
%token UNDERSCORE
%token GET
%token FILL
%token CONVERT
%token SAT
%token PROMOTE_F32
%token SIZE
%token SET
%token IS_NULL
%token LOCAL
%token NULL
%token REINTERPRET
%token GLOBAL
%token ELEM
%token STORE8 STORE16 STORE STORE32
%token BRTABLE
%token CALL
%token LOAD LOAD8
%token LOAD16
%token LOOP
%token DATA
%token BRIF BR
%token OFFSET
%token UNREACHABLE
%token CALL_INDIRECT
%token LOAD32
%token BLOCK ALIGN EQUAL MEMORY RETURN NOP
%token FUNC
%token EXPORT IMPORT
%token EXTERN
%token MUTABLE
%token MODULE
%token RPAR
%token LPAR
%token EOF
%token IF ELSE THEN
%token DOT
%token CONST
%token START TYPE
%token<string> NAT

%{
open Types

type p_module_field =
  | MType of type_
  | MGlobal of global
  | MTable of table
  | MMem of mem
  | MFunc of func
  | MElem of elem
  | MData of data
  | MStart of start
  | MImport of import
  | MExport of export

type p_param_or_result =
  | TmpParam of param
  | TmpResult of result_

let nat32 s =
  try Int32.of_string s with Failure _ -> failwith "error i32 constant out of range"

%}

%start <Types.module_> module_
%type <Types.ibinop> ibinop
%type <Types.fbinop> fbinop

%%

(* Helpers *)

let par(X) ==
  | LPAR; ~ = X; RPAR; <>

(* Types *)

let ref_kind ==
  | FUNC; { Func_ref }
  | EXTERN; { Extern_ref }

let ref_type ==
  | FUNC_REF; { Func_ref }
  | EXTERN_REF; { Extern_ref }

let val_type ==
  | ~ = num_type; <Num_type>
  | ~ = ref_type; <Ref_type>

let global_type ==
  | t = val_type; { Const, t }
  | t = par(preceded(MUTABLE, val_type)); { Var, t }

let def_type ==
  | ~ = par(preceded(FUNC, func_type)); <>

let func_type :=
  | o = list(par(preceded(RESULT, list(val_type)))); { [], o }
  | LPAR; PARAM; option(id); i = list(val_type); RPAR; x = func_type; {
    let i', o = x in
    i @ i', o
  }

let table_type ==
  | ~ = limits; ~ = ref_type; { limits, ref_type }

let mem_type ==
  | ~ = limits; <>

let limits ==
  | min = NAT; { min = nat32; max = None }
  | min = NAT; max = NAT; { min = nat32 min; max = Some (nat32 max) }

let type_use ==
  | ~ = par(preceded(TYPE, indice)); <>

(* Immediates *)

let num ==
  | ~ = NAT; <>
  | ~ = INT; <>
  | ~ = FLOAT; <>

(* var *)
let indice ==
  | ~ = ID; <Symbolic>
  | ~ = U32; <Raw>

(* bind_var *)
let id ==
  | ~ = ID; <>

let func_idx ==
  | ~ = indice; <>
let elem_idx ==
  | ~ = indice; <>
let table_idx ==
  | ~ = indice; <>
let local_idx ==
  | ~ = indice; <>
let data_idx ==
  | ~ = indice; <>
let global_idx ==
  | ~ = indice; <>
let label_idx ==
  | ~ = indice; <>
let type_idx ==
  | ~ = indice; <>

(* TODO: TO CLASSIFY *)


let export_desc ==
  | FUNC; ~ = func_idx; <Func>

let num_type ==
  | I32; { Types.I32 }
  | I64; { Types.I64 }
  | F32; { Types.F32 }
  | F64; { Types.F64 }

let inn ==
  | I32; { Types.S32 }
  | I64; { Types.S64 }

let result ==
 | RESULT; ~ = val_type; <>

let fnn ==
  | F32; { Types.S32 }
  | F64; { Types.S64 }

let iunop ==
  | CLZ; { Clz }
  | CTZ; { Ctz }
  | POPCNT; { Popcnt }

let funop ==
  | ABS; { Abs }
  | NEG; { Neg }
  | SQRT; { Sqrt }
  | CEIL; { Ceil }
  | FLOOR; { Floor }
  | TRUNC; { Trunc }
  | NEAREST; { Nearest }

let sx ==
  | SIGNED; { S }
  | UNSIGNED; { U }

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

let fbinop ==
  | ADD; { Add }
  | SUB; { Sub }
  | MUL; { Mul }
  | DIV; { Div }
  | MIN; { Min }
  | MAX; { Max }
  | COPYSIGN; { Copysign }

let itestop ==
  | EQZ; { Eqz }

let irelop ==
  | EQ; { (Eq : Types.irelop) }
  | NE; { (Ne : Types.irelop) }
  | LT; s = sx; { (Lt s: Types.irelop) }
  | GT; s = sx; { (Gt s: Types.irelop) }
  | LE; s = sx; { (Le s: Types.irelop) }
  | GE; s = sx; { (Ge s: Types.irelop) }

let frelop ==
  | EQ; { Eq }
  | NE; { Ne }
  | LT; { Lt }
  | GT; { Gt }
  | LE; { Le }
  | GE; { Ge }

let memarg ==
  | OFFSET; EQUAL; offset = U32; ALIGN; EQUAL; align = U32; { {offset = offset; align = align} }

let block_type ==
  | LPAR; RESULT; ~ = type_idx; LPAR; <Type_idx>
  | LPAR; RESULT; t = val_type; RPAR; { Val_type (Some t) }
  | { Val_type None }

let else_ :=
  | LPAR; ELSE; ~ = expr; RPAR; <>
  | { [] }

let then_ :=
  | LPAR; THEN; ~ = expr; RPAR; <>
  | i = instr; { [i] }

let instr :=
  | I32; DOT; CONST; i = INT; { I32_const (Int64.to_int32 i) }
  | I64; DOT; CONST; ~ = INT; <I64_const>
  | F32; DOT; CONST; ~ = FLOAT; <F32_const>
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
  | I32; DOT; WRAPI64; { I32_wrap_i64 }
  | I64; DOT; EXTEND_I32; ~ = sx; <I64_extend_i32>
  | ~ = inn; DOT; TRUNC; UNDERSCORE; ~ = fnn; s = sx; <I_trunc_f>
  | ~ = inn; DOT; TRUNC; UNDERSCORE; SAT; UNDERSCORE; ~ = fnn; s = sx; <I_trunc_sat_f>
  | F32; DOT; DEMOTE_F64; { F32_demote_f64 }
  | F64; DOT; PROMOTE_F32; { F64_promote_f32 }
  | ~ = fnn; DOT; CONVERT; UNDERSCORE; ~ = inn; s = sx; <F_convert_i>
  | ~ = inn; DOT; REINTERPRET; UNDERSCORE; ~ = fnn; <I_reinterpret_f>
  | ~ = fnn; DOT; REINTERPRET; UNDERSCORE; ~ = inn; <F_reinterpret_i>
  | REF; DOT; NULL; ~ = ref_type; <Ref_null>
  | REF; DOT; IS_NULL; { Ref_is_null }
  | REF; FUNC; ~ = func_idx; <Ref_func>
  | DROP; { Drop }
  | SELECT; ~ = option(par(list(par(result)))); <Select>
  | LOCAL; DOT; GET; ~ = local_idx; <Local_get>
  | LOCAL; DOT; SET; ~ = local_idx; <Local_set>
  | LOCAL; DOT; TEE; ~ = local_idx; <Local_tee>
  | GLOBAL; DOT; GET; ~ = global_idx; <Global_get>
  | GLOBAL; DOT; SET; ~ = global_idx; <Global_set>
  | TABLE; DOT; GET; ~ = table_idx; <Table_get>
  | TABLE; DOT; SET; ~ = table_idx; <Table_set>
  | TABLE; DOT; SIZE; ~ = table_idx; <Table_size>
  | TABLE; DOT; GROW; ~ = table_idx; <Table_grow>
  | TABLE; DOT; FILL; ~ = table_idx; <Table_fill>
  | TABLE; DOT; COPY; src = table_idx; dst = table_idx; { Table_copy (src, dst) }
  | TABLE; DOT; INIT; ~ = table_idx; ~ = elem_idx; <Table_init>
  | ELEM; DOT; DROP; ~ = elem_idx; <Elem_drop>
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
  | MEMORY; DOT; INIT; ~ = data_idx; <Memory_init>
  | DATA; DOT; DROP; ~ = data_idx; <Data_drop>
  | NOP; { Nop }
  | UNREACHABLE; { Unreachable }
  | BLOCK; { assert false }
  | LOOP; { assert false }
  | IF; (*_bt = option(block_type);*) _cond = par(instr);
    i = then_;
    i2 = else_; {
      If_else (Val_type None, i, i2)
    }
  | BR; ~ = label_idx; <Br>
  | BRIF; ~ = label_idx; <Br_if>
  | BRTABLE; { assert false }
  | RETURN; { Return }
  | CALL; ~ = func_idx; <Call>
  | CALL_INDIRECT; { assert false }
  | ~ = par(instr); <>

let expr :=
  | ~ = list(instr); <>

let param_or_result :=
  | PARAM; id = option(id); t = val_type; {
    let res = id, t in
    TmpParam res
  }
  | r = result; {
    let res = r in
    TmpResult res
  }

let func_field :=
  | FUNC; id = option(id); sig_ = list(par(param_or_result)); body = expr; {
    let (params, results) = List.fold_left (fun (params, results) -> function
      | TmpParam p -> if results <> [] then failwith "param after result" else (p::params, [])
      | TmpResult r -> (params, r::results)
    ) ([], []) sig_ in
    let _results = List.rev results in
    {
      locals = List.rev params;
      body = body;
      id = id;
    }
  }

let import_field :=
  | IMPORT; {}

let export_field :=
  | EXPORT; n = NAME; d = par(export_desc); { { name = n; desc = d; } }

(* Modules *)

let type_ ==
  | ~ = def_type; <>

let type_def ==
  | LPAR; id = option(id); TYPE; t = type_; RPAR; { id, t }

let start ==
  | ~ = par(preceded(START, func_idx)); <>

let module_field :=
  | ~ = type_def; <MType>
  | ~ = export_field; <MExport>
  | ~ = func_field; <MFunc>
  | ~ = start; <MStart>

let module_ :=
  | LPAR; MODULE; id = option(id); fields = list(par(module_field)); RPAR; EOF; {
    let res = List.fold_left (fun m f ->
      match f with
      | MExport e -> { m with exports = e::m.exports }
      | MFunc f -> { m with funcs = f::m.funcs }
      | MStart start ->
        if Option.is_some m.start then failwith "multiple start sections"
        else { m with start = Some start }
      | MImport i ->
        if m.funcs <> [] then failwith "import after function definition"
        else if m.mems <> [] then failwith "import after memory definition"
        else if m.tables <> [] then failwith "import after table definition"
        else if m.globals <> [] then failwith "import after global definition"
        else { m with imports = i::m.imports }
      | MData d -> { m with datas = d::m.datas }
      | MElem e -> { m with elems = e::m.elems }
      | MMem mem -> { m with mems = mem::m.mems }
      | MType t -> { m with types = t::m.types }
      | MGlobal g -> { m with globals = g::m.globals }
      | MTable t -> { m with tables = t::m.tables }
      ) {
        id = id;
        types = [];
        funcs = [];
        tables = [];
        mems = [];
        globals = [];
        elems = [];
        datas = [];
        start = None;
        imports = [];
        exports = [];
      }
      fields
    in
    { res with
      types = List.rev res.types;
      funcs = List.rev res.funcs;
      tables = List.rev res.tables;
      mems = List.rev res.mems;
      globals = List.rev res.globals;
      elems = List.rev res.elems;
      datas = List.rev res.datas;
      imports = List.rev res.imports;
      exports = List.rev res.exports;
    }
  }
