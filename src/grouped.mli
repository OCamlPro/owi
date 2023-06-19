type opt_ind =
  | Curr of int
  | Indice of Symbolic.indice

type opt_export =
  { name : string
  ; id : opt_ind
  }

type opt_exports =
  { global : opt_export list
  ; mem : opt_export list
  ; table : opt_export list
  ; func : opt_export list
  }

type type_check = Symbolic.indice * Symbolic.func_type

type t =
  { id : string option
  ; typ : Symbolic.type_def list
  ; function_type : Symbolic.func_type list
      (* Types comming from function declarations.
         It contains potential duplication *)
  ; type_checks : type_check list
      (* Types checks to perform after assignment.
         Come from function declarations with type indicies *)
  ; global : (Symbolic.global, Simplified.global_type) Runtime.t Indexed.t list
  ; table : (Simplified.table, Simplified.table_type) Runtime.t Indexed.t list
  ; mem : (Types.mem, Types.limits) Runtime.t Indexed.t list
  ; func : (Symbolic.func, Symbolic.block_type) Runtime.t Indexed.t list
  ; elem : Symbolic.elem Indexed.t list
  ; data : Symbolic.data Indexed.t list
  ; exports : opt_exports
  ; start : Symbolic.indice option
  }

val convert_param : Symbolic.param -> Simplified.param

val convert_ref_type : Symbolic.ref_type -> Simplified.ref_type

val convert_val_type : Symbolic.val_type -> Simplified.val_type

val convert_func_type : Symbolic.func_type -> Simplified.func_type

val convert_str : Symbolic.str_type -> Simplified.str_type

val convert_heap_type : Symbolic.heap_type -> Simplified.heap_type

val of_symbolic : Symbolic.modul -> t Result.t
