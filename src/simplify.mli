open Types

type 'a runtime =
  | Local of 'a
  | Imported of int * indice

type runtime_table =
  (ref_type * (int * const) option array * int option) runtime

type runtime_global = (global_type * const) runtime

type runtime_memory = (bytes * int option) runtime

type runtime_func = func runtime

type module_ =
  { fields : module_field list
  ; seen_funcs : (string, int) Hashtbl.t
  ; datas : string array
  ; funcs : runtime_func array
  ; memories : runtime_memory array
  ; tables : runtime_table array
  ; globals : runtime_global array
  ; globals_tmp : (global_type * expr) runtime array
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
