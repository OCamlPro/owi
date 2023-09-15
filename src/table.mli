(** runtime table *)
type table = Value.ref_value array

type t =
  { id : int
  ; label : string option
  ; limits : Types.limits
  ; typ : Simplified.ref_type
  ; mutable data : table
  }

val get : t -> int -> Value.ref_value

val set : t -> int -> Value.ref_value -> unit

val size : t -> int

val typ : t -> Simplified.ref_type

val update : t -> table -> unit

val init : ?label:string -> Simplified.table_type -> t

val max_size : t -> int option

val grow : t -> int32 -> Value.ref_value -> unit
