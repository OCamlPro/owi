(** Module to typecheck a simplified module. *)

(** typecheck a given module *)
val modul : Types.Simplified.modul -> (unit, string) Result.t
