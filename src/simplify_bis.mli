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

type result =
  { id : string option
  ; global : ((index, Const.expr) global', global_import) runtime named
  ; table : (table, table_import) runtime named
  ; mem : (mem, mem_import) runtime named
  ; func : ((index, func_type) func', func_type) runtime named
  ; elem : (index, Const.expr) elem' named
  ; data : (index, Const.expr) data' named
  ; export : index export' list
  ; start : index list
  }

val simplify : Types.module_ -> result
