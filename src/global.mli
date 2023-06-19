(** runtime global *)
type 'env t =
  { mutable value : 'env Value.t
  ; label : string option
  ; mut : Types.mut
  ; typ : Simplified.val_type
  }
