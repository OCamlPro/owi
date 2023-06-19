(** Module to simplify a text module. It contains the types of simplified
    modules. *)

val convert_heap_type : Types.Symbolic.heap_type -> Types.Simplified.heap_type

(** simplify a module *)
val modul : Types.Symbolic.modul -> Types.Simplified.modul Result.t
