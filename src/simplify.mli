open Types

type ('a, 'b) runtime =
  | Local of 'a
  | Imported of int * indice * 'b

type runtime_table =
  (ref_type * (int * const) option array * int option, unit) runtime

type runtime_global = (global_type * const, global_type) runtime

type runtime_memory = (bytes * int option, unit) runtime

type runtime_func = (func, unit) runtime

type module_ =
  { fields : module_field list
  ; datas : string array
  ; funcs : runtime_func array
  ; memories : runtime_memory array
  ; tables : runtime_table array
  ; globals : runtime_global array
  ; globals_tmp : (global_type * indice expr, global_type) runtime array
  ; types : func_type array
  ; elements : (ref_type * const array) array
  ; exported_funcs : (string, int) Hashtbl.t
  ; exported_globals : (string, int) Hashtbl.t
  ; exported_memories : (string, int) Hashtbl.t
  ; exported_tables : (string, int) Hashtbl.t
  ; start : int option
  ; should_trap : string option
  ; should_not_link : string option
  }

val find_module : string option -> 'a option -> (string, 'a) Hashtbl.t -> 'a

val mk_module : (string, int) Hashtbl.t -> Types.module_ -> module_
