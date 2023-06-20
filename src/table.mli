(** runtime table *)
type 'env table = 'env Value.ref_value array

type 'env t =
  { id : int
  ; label : string option
  ; limits : Types.limits
  ; typ : Simplified.ref_type
  ; mutable data : 'env table
  }

val update : 'a t -> 'a table -> unit

val init : ?label:string -> Simplified.table_type -> 'env t