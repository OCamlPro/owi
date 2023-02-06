(** Module to typecheck a simplified module. *)

(** typecheck a given module *)
val module_ : Types.Simplified.modul -> (unit, string) Result.t
