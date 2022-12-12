(** Module to typecheck a simplified module. *)

(** typecheck a given module *)
val module_ : Simplify.simplified_module -> (unit, string) Result.t
