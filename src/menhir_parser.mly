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
%token BLOCK ALIGN EQUAL IFELSE MEMORY RETURN NOP
%token FUNC
%token EXPORT
%token MODULE
%token RPAR
%token LPAR
%token EOF
%token IF
%token DOT
%token CONST

%{
open Types

type p_module_field =
  | MExport of export
  | MFunc of func

type p_param_or_result =
  | TmpParam of param
  | TmpResult of result_

let log = Format.eprintf
%}

%start <Types.module_> module_
%type <Types.ibinop> ibinop
%type <Types.fbinop> fbinop

%%

let par(X) ==
  | LPAR; x = X; RPAR; { x }

let id ==
  | id = ID; {
    let id = String.sub id 1 (String.length id - 1) in
    let res = id in
    log "PARSED ID %a@." Pp.id res;
    res
  }

let indice ==
  | id = ID; {
    let id = String.sub id 1 (String.length id - 1) in
    let res = Symbolic id in
    log "PARSED INDICE %a@." Pp.indice res;
    res
  }
  | u = U32; {
    let res = Raw u in
    log "PARSED INDICE %a@." Pp.indice res;
    res
  }

let func_idx ==
  | i = indice; {
    let res = i in
    log "PARSED FUNC_IDX %a@." Pp.func_idx res;
    res
  }

let export_desc ==
  | FUNC; f = func_idx; {
    let res = Func f in
    log "PARSED EXPORT_DESC %a@." Pp.export_desc res;
    res
  }

let num_type ==
  | I32; {
    let res = Types.I32 in
    log "PARSED NUMTYPE %a@." Pp.num_type res;
    res
  }
  | I64; {
    let res = Types.I64 in
    log "PARSED NUMTYPE %a@." Pp.num_type res;
    res
  }
  | F32; {
    let res = Types.F32 in
    log "PARSED NUMTYPE %a@." Pp.num_type res;
    res
  }
  | F64; {
    let res = Types.F64 in
    log "PARSED NUMTYPE %a@." Pp.num_type res;
    res
  }

let ref_type ==
  | FUNC_REF; {
    let res = Func_ref in
    log "PARSED REF_TYPE %a@." Pp.ref_type res;
    res
  }
  | EXTERN_REF; {
    let res = Extern_ref in
    log "PARSED REF_TYPE %a@." Pp.ref_type res;
    res
  }

let val_type ==
  | nt = num_type; {
    let res = Num_type nt in
    log "PARSED VAL_TYPE %a@." Pp.val_type res;
    res
  }
  | rt = ref_type; {
    let res = Ref_type rt in
    log "PARSED VAL_TYPE@ %a@." Pp.val_type res;
    res
  }

let inn ==
  | I32; { Types.S32 }
  | I64; { Types.S64 }

let result ==
 | RESULT; t = val_type; { t }

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

let elem_idx ==
  | i = indice; { i }

let table_idx ==
  | i = indice; { i }

let local_idx ==
  | i = indice; { i }

let data_idx ==
  | i = indice; { i }

let global_idx ==
  | i = indice; { i }

let label_idx ==
  | i = indice; { i }

let memarg ==
  | OFFSET; EQUAL; offset = U32; ALIGN; EQUAL; align = U32; { {offset = offset; align = align} }

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
  | IFELSE; { assert false }
  | BR; ~ = label_idx; <Br>
  | BRIF; ~ = label_idx; <Br_if>
  | BRTABLE; { assert false }
  | RETURN; { Return }
  | CALL; ~ = func_idx; <Call>
  | CALL_INDIRECT; { assert false }
  | i = par(instr); { i }

let param_or_result :=
  | PARAM; id = option(id); t = val_type; {
    let res = id, t in
    log "PARSED PARAM %a@." Pp.param res;
    TmpParam res
  }
  | r = result; {
    let res = r in
    log "PARSED RESULT %a@." Pp.result_ res;
    TmpResult res
  }

let module_field :=
  | EXPORT; n = NAME; d = par(export_desc); {
    let res = { name = n; desc = d } in
    log "PARSED MODULE FIELD %a@." Pp.export res;
    MExport res
  }
  | FUNC; id = option(id); sig_ = list(par(param_or_result)); body = list(par(instr)); {
    let (params, results) = List.fold_left (fun (params, results) -> function
      | TmpParam p -> if results <> [] then failwith "param after result" else (p::params, [])
      | TmpResult r -> (params, r::results)
    ) ([], []) sig_ in
    let params = List.rev params in
    let _results = List.rev results in
    let res = {
      locals = params;
      body = body;
      id = id;
    }
    in
    log "PARSED MODULE FIELD %a@." Pp.func res;
    MFunc res
  }

let module_ :=
  | LPAR; MODULE; id = option(id); fields = list(par(module_field)); RPAR; EOF; {
    let res = List.fold_left (fun m f ->
      match f with
      | MExport e -> { m with exports = e::m.exports }
      | MFunc f -> { m with funcs = f::m.funcs }
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
    Format.eprintf "PARSED MODULE %a@." Pp.module_ res;
    res
  }
