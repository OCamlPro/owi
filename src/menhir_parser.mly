%token<string> ID
%token<string> NAME
%token<Unsigned.UInt32.t> U32
%token PARAM RESULT
%token FUNC_REF EXTERN_REF
%token I32 I64 F32 F64
%token FUNC
%token EXPORT
%token MODULE
%token RPAR
%token LPAR
%token EOF

%{
open Types

type p_module_field =
  | Export of export
  | Func of func

      %}

%start <Types.module_> module_

%%

  let par(X) :=
      | LPAR; x = X; RPAR; { x }

let id :=
    id = ID; { id }

let indice :=
    | id = ID; { Symbolic id }
             | u = U32; { Raw u }

let func_idx :=
    i = indice; { i }

let export_desc :=
    | FUNC; f = func_idx; { Func f }

let num_type :=
    | I32; { I32 }
         | I64; { I64 }
              | F32; { F32 }
                   | F64; { F64 }

let ref_type :=
    | FUNC_REF; { Func_ref }
              | EXTERN_REF; { Extern_ref }

let val_type :=
    | nt = num_type; { Num_type nt }
                   | rt = ref_type; { Ref_type rt }

let param :=
    PARAM; id = option(id); t = val_type; { t, id }

let result :=
    RESULT; t = val_type; { t }

let body :=
    { [] }

let module_field :=
    | EXPORT; n = NAME; LPAR; d = export_desc; RPAR; {
  Export ({name = n; desc = d})
}
                                                   | FUNC; id = option(id); p = list(par(param)); _r = list(par(result)); b = body; {
    Func ({
      locals = p;
      body = b;
      id = id;
    })
  }

let module_ :=
    LPAR; MODULE; id = option(id); fields = list(par(module_field)); RPAR; EOF; {
  List.fold_left (fun m f ->
    match f with
    | Export e -> { m with exports = e::m.exports }
    | Func f -> { m with funcs = f::m.funcs }
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
}
