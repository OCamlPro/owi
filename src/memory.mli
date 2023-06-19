(** runtime memory *)
type t

val get_data : t -> bytes

val get_limit_max : t -> int option

val get_limits : t -> Types.limits

val init : ?label:string -> Types.limits -> t

val update_memory : t -> bytes -> unit
