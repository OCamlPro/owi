(** Module to simplify a text module. It contains the types of simplified
    modules. *)

val convert_heap_type : Symbolic.heap_type -> Types.Simplified.heap_type

(** simplify a module *)
val modul : Symbolic.modul -> Types.Simplified.modul Result.t
