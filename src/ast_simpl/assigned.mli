open Types

type t =
  { id : string option
  ; typ : simplified str_type Named.t
  ; global : (Text.global, simplified global_type) Runtime.t Named.t
  ; table : (simplified table, simplified table_type) Runtime.t Named.t
  ; mem : (Types.mem, Types.limits) Runtime.t Named.t
  ; func : (text func, text block_type) Runtime.t Named.t
  ; elem : Text.elem Named.t
  ; data : Text.data Named.t
  ; exports : Grouped.opt_exports
  ; start : text indice option
  }

val of_grouped : Grouped.t -> t Result.t
