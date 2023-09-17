type opt_ind =
  | Curr of int
  | Indice of Text.indice

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

type type_check = Text.indice * Text.func_type

type t =
  { id : string option
  ; typ : Text.type_def list
  ; function_type : Text.func_type list
      (* Types comming from function declarations.
         It contains potential duplication *)
  ; type_checks : type_check list
      (* Types checks to perform after assignment.
         Come from function declarations with type indicies *)
  ; global : (Text.global, Simplified.global_type) Runtime.t Indexed.t list
  ; table : (Simplified.table, Simplified.table_type) Runtime.t Indexed.t list
  ; mem : (Types.mem, Types.limits) Runtime.t Indexed.t list
  ; func : (Text.func, Text.block_type) Runtime.t Indexed.t list
  ; elem : Text.elem Indexed.t list
  ; data : Text.data Indexed.t list
  ; exports : opt_exports
  ; start : Text.indice option
  }

val of_symbolic : Text.modul -> t Result.t
