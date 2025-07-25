(* TODO: hide the definition completely to force proper propagation of values *)
type 'a t = private
  { raw : 'a
  ; instr_counter : int Atomic.t
  ; mutable functions_called : int list
  ; mutable distances : int array
  ; mutable d_true : int array
  ; mutable d_false : int array
  }

val dummy : 'a -> 'a t

val dummies : 'a list -> 'a t list

val dummy_deep : 'a list -> 'a t list t

val map : ('a -> 'b) -> 'a t -> 'b t

val iter : ('a -> Unit.t) -> 'a t -> Unit.t

val update_functions_called : 'a t -> int list -> unit

val init_distances : 'a t -> int array -> unit

val init_d_true : 'a t -> int array -> unit

val init_d_false : 'a t -> int array -> unit
