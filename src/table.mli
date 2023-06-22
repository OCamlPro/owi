(** runtime table *)
type 'env table = Value.ref_value array

type 'env t =
  { id : int
  ; label : string option
  ; limits : Types.limits
  ; typ : Simplified.ref_type
  ; mutable data : 'env table
  }

val get : 'env t -> int32 -> Value.ref_value
val set : 'env t -> int32 -> Value.ref_value -> unit
val size : 'env t -> int32

val update : 'a t -> 'a table -> unit

val init : ?label:string -> Simplified.table_type -> 'env t
