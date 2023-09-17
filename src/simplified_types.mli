type tbl = (string, int) Hashtbl.t Option.t

val equal_func_types : Simplified.func_type -> Simplified.func_type -> bool

val convert_val_type : tbl -> Text.val_type -> Simplified.val_type Result.t

val convert_heap_type : tbl -> Text.heap_type -> Simplified.heap_type Result.t

val convert_func_type : tbl -> Text.func_type -> Simplified.func_type Result.t

val convert_ref_type : tbl -> Text.ref_type -> Simplified.ref_type Result.t

val convert_param : tbl -> Text.param -> Simplified.param Result.t

val convert_table_type :
  tbl -> Text.table_type -> Simplified.table_type Result.t

val convert_str : tbl -> Text.str_type -> Simplified.str_type Result.t
