(** runtime global *)
type t =
  { mutable value : Value.t
  ; label : string option
  ; mut : Types.mut
  ; typ : Simplified.val_type
  }

val value : t -> Value.t

val set_value : t -> Value.t -> unit

val typ : t -> Simplified.val_type

val mut : t -> Types.mut
