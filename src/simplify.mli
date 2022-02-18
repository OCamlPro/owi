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
  }

type action =
  | Invoke_indice of int * string * const list
  | Get_indice of int * string

type assert_ =
  | SAssert_return of action * result list
  | SAssert_trap of action * string
  | SAssert_exhaustion of action * string
  | SAssert_invalid of Types.module_ * string
  | SAssert_invalid_quote of string list * string
  | SAssert_invalid_binary of string list * string
  | SAssert_unlinkable of Types.module_ * string

type cmd =
  | Module_indice of int
  | Assert of assert_
  | Register_indice of string * int
  | Action of action

type script = cmd list * module_ Array.t

val script : Types.file -> script
