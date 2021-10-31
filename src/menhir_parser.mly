%token<string> ID
%token<string> NAME
%token<Unsigned.UInt32.t> U32
%token<Int64.t> INT
%token PARAM RESULT
%token FUNC_REF EXTERN_REF
%token I32 I64 F32 F64
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

%%

  let par(X) ==
      | LPAR; x = X; RPAR; { x }

let id ==
    id = ID; {
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
    res }

let func_idx ==
    i = indice; {
  let res = i in
  log "PARSED FUNC_IDX %a@." Pp.func_idx res;
  res }

let export_desc ==
    | FUNC; f = func_idx; {
  let res = Func f in
  log "PARSED EXPORT_DESC %a@." Pp.export_desc res;
  res }

let num_type ==
    | I32; {
    let res = Types.I32 in
    log "PARSED NUMTYPE %a@." Pp.num_type res;
    res }
         | I64; {
    let res = Types.I64 in
    log "PARSED NUMTYPE %a@." Pp.num_type res;
    res }
              | F32; {
    let res = Types.F32 in
    log "PARSED NUMTYPE %a@." Pp.num_type res;
    res }
                   | F64; {
    let res = Types.F64 in
    log "PARSED NUMTYPE %a@." Pp.num_type res;
    res }

let ref_type ==
    | FUNC_REF; {
    let res = Func_ref in
    log "PARSED REF_TYPE %a@." Pp.ref_type res;
    res }
              | EXTERN_REF; {
    let res = Extern_ref in
    log "PARSED REF_TYPE %a@." Pp.ref_type res;
    res }

let val_type ==
    | nt = num_type; {
  let res = Num_type nt in
  log "PARSED VAL_TYPE %a@." Pp.val_type res;
  res }
                   | rt = ref_type; {
    let res = Ref_type rt in
    log "PARSED VAL_TYPE@ %a@." Pp.val_type res;
    res }

let instr :=
    | I32; DOT; CONST; {}

let param_or_result :=
    | PARAM; id = option(id); t = val_type; {
  let res = id, t in
  log "PARSED PARAM %a@." Pp.param res;
  TmpParam res }
                                          | RESULT; t = val_type; {
    let res = t in
    log "PARSED RESULT %a@." Pp.result_ res;
    TmpResult res }

let module_field :=
    | EXPORT; n = NAME; d = par(export_desc); {
  let res = { name = n; desc = d } in
  log "PARSED MODULE FIELD %a@." Pp.export res;
  MExport res
}
                                            | FUNC; id = option(id); sig_ = list(par(param_or_result)); {
    let (params, results) = List.fold_left (fun (params, results) -> function
      | TmpParam p -> if results <> [] then failwith "param after result" else (p::params, [])
      | TmpResult r -> (params, r::results)
    ) ([], []) sig_ in
    let params = List.rev params in
    let _results = List.rev results in
    let res = {
      locals = params;
      body = [];
      id = id;
    }
    in
    log "PARSED MODULE FIELD %a@." Pp.func res;
    MFunc res
  }

let module_ :=
    LPAR; MODULE; id = option(id); fields = list(par(module_field)); RPAR; EOF; {
  let res = List.fold_left (fun m f ->
    match f with
    | MExport e -> { m with exports = e::m.exports }
    | MFunc f -> { m with funcs = f::m.funcs }
  )
    {
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
