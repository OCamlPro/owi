(** Module to simplify a text module. It contains the types of simplified
    modules. *)

val convert_heap_type : Symbolic.heap_type -> Simplified.heap_type

(** simplify a module *)
val modul : Symbolic.modul -> Simplified.modul Result.t
