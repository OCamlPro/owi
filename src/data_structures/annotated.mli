(* TODO: hide the definition completely to force proper propagation of values *)
type 'a t = private
  { raw : 'a
  ; nb_iter : int Atomic.t
  }

val dummy : 'a -> 'a t

val dummies : 'a list -> 'a t list

val dummy_deep : 'a list -> 'a t list t

val map : ('a -> 'b) -> 'a t -> 'b t

val iter : ('a -> Unit.t) -> 'a t -> Unit.t
