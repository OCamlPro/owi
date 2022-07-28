type table_import = Types.table_type
type mem_import = Types.limits
type global_import = Types.global_type

type 'a imp =
  { module_ : string
  ; name : string
  ; assigned_name : string option
  ; desc : 'a
  }

type ('a, 'b) runtime =
  | Local of 'a
  | Imported of 'b imp

module StringMap :
  Map.S
    with type key = Map.Make(String).key
     and type 'a t = 'a Map.Make(String).t

type index = Types.simplified_indice

type 'a indexed =
  { index : index
  ; value : 'a
  }

type 'a named =
  { values : 'a indexed list
  ; named : index StringMap.t
  }

type ('indice, 'bt) module_with_index =
  { id : string option
  ; type_ : Types.func_type named
  ; global : (('indice, 'bt) Types.global', global_import) runtime named
  ; table : (Types.table, table_import) runtime named
  ; mem : (Types.mem, mem_import) runtime named
  ; func : (('indice, 'bt) Types.func', 'bt) runtime named
  ; elem : ('indice, 'bt) Types.elem' named
  ; data : ('indice, 'bt) Types.data' named
  ; export : 'indice Types.export' list
  ; start : 'indice list
  }

type assigned_module =
  (Types.indice, Types.indice Types.block_type) module_with_index

type rewritten_module = (index, Types.func_type) module_with_index

val simplify : Types.module_ -> rewritten_module
