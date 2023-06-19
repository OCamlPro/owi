type tbl = (string, int) Hashtbl.t Option.t

val equal_func_types : Simplified.func_type -> Simplified.func_type -> bool

val convert_val_type : tbl -> Symbolic.val_type -> Simplified.val_type

val convert_heap_type : tbl -> Symbolic.heap_type -> Simplified.heap_type

val convert_func_type : tbl -> Symbolic.func_type -> Simplified.func_type

val convert_ref_type : tbl -> Symbolic.ref_type -> Simplified.ref_type

val convert_param : tbl -> Symbolic.param -> Simplified.param

val convert_table_type : tbl -> Symbolic.table_type -> Simplified.table_type

val convert_str : tbl -> Symbolic.str_type -> Simplified.str_type
