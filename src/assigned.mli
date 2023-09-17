type t =
  { id : string option
  ; typ : Simplified.str_type Named.t
  ; global : (Text.global, Simplified.global_type) Runtime.t Named.t
  ; table : (Simplified.table, Simplified.table_type) Runtime.t Named.t
  ; mem : (Types.mem, Types.limits) Runtime.t Named.t
  ; func : (Text.func, Text.block_type) Runtime.t Named.t
  ; elem : Text.elem Named.t
  ; data : Text.data Named.t
  ; exports : Grouped.opt_exports
  ; start : Text.indice option
  }

val of_grouped : Grouped.t -> t Result.t
