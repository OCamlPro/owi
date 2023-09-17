(** runtime global *)
type t =
  { mutable value : Concrete_value.t
  ; label : string option
  ; mut : Types.mut
  ; typ : Simplified.val_type
  }

val value : t -> Concrete_value.t

val set_value : t -> Concrete_value.t -> unit

val typ : t -> Simplified.val_type

val mut : t -> Types.mut
