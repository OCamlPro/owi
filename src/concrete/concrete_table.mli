(** runtime table *)

open Types

type table = Concrete_value.ref_value array

type t =
  { id : int
  ; label : string option
  ; limits : limits
  ; typ : simplified ref_type
  ; mutable data : table
  }

val get : t -> int -> Concrete_value.ref_value

val set : t -> int -> Concrete_value.ref_value -> unit

val size : t -> int

val typ : t -> simplified ref_type

val update : t -> table -> unit

val init : ?label:string -> simplified table_type -> t

val max_size : t -> int option

val grow : t -> int32 -> Concrete_value.ref_value -> unit

val fill : t -> int32 -> int32 -> Concrete_value.ref_value -> unit

val copy : t_src:t -> t_dst:t -> src:int32 -> dst:int32 -> len:int32 -> unit
