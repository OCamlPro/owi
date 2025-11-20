(* TODO: hide the definition completely to force proper propagation of values *)
type 'a t = private
  { raw : 'a
  ; instr_counter : int Atomic.t
  ; mutable functions_called : Set.Make(Int).t
  ; d_true : int array option ref
  ; d_false : int array option ref
  }

val dummy : 'a -> 'a t

val dummies : 'a list -> 'a t list

val dummy_deep : 'a list -> 'a t list t

val map : ('a -> 'b) -> 'a t -> 'b t

val iter : ('a -> Unit.t) -> 'a t -> Unit.t

val update_functions_called : 'a t -> Set.Make(Int).t -> unit

val set_d_true : 'a t -> int array -> unit

val set_d_false : 'a t -> int array -> unit

val raw : 'a t -> 'a
