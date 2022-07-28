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

open Types

type ('indice, 'bt) module_with_index =
  { id : string option
  ; type_ : func_type named
  ; global :
      (('indice, ('indice, 'bt) expr') global', global_import) runtime named
  ; table : (table, table_import) runtime named
  ; mem : (mem, mem_import) runtime named
  ; func : (('indice, 'bt) func', 'bt) runtime named
  ; elem : ('indice, ('indice, 'bt) expr') elem' named
  ; data : ('indice, ('indice, 'bt) expr') data' named
  ; export : 'indice export' list
  ; start : 'indice list
  }

type rewritten_module = (index, func_type) module_with_index

val simplify : Types.module_ -> rewritten_module
