type t =
  { id : string option
  ; typ : Simplified.str_type Named.t
  ; global : (Symbolic.global, Simplified.global_type) Runtime.t Named.t
  ; table : (Simplified.table, Simplified.table_type) Runtime.t Named.t
  ; mem : (Types.mem, Types.limits) Runtime.t Named.t
  ; func : (Symbolic.func, Symbolic.block_type) Runtime.t Named.t
  ; elem : Symbolic.elem Named.t
  ; data : Symbolic.data Named.t
  ; exports : Grouped.opt_exports
  ; start : Symbolic.indice option
  }

val of_grouped : Grouped.t -> t Result.t
