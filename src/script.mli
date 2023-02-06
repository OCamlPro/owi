(** Module to execute a full Wasm script. *)

(** execute a Wasm script *)
val exec :
  ?with_exhaustion:bool -> Types.Symbolic.script -> (unit, string) result
