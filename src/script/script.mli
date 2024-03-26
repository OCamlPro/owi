(** Module to execute a full Wasm script. *)

(** execute a Wasm script *)
val exec : no_exhaustion:bool -> optimize:bool -> Text.script -> unit Result.t
