(** runtime global *)
type 'env t =
  { mutable value : 'env Value.t
  ; label : string option
  ; mut : Types.mut
  ; typ : Simplified.val_type
  }

val value : 'env t -> 'env Value.t

val set_value : 'env t -> 'env Value.t -> unit

val typ : 'env t -> Simplified.val_type

val mut : 'env t -> Types.mut
