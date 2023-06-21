(** runtime memory *)
type t

val get_data : t -> bytes

val get_limit_max : t -> int option

val get_limits : t -> Types.limits

val init : ?label:string -> Types.limits -> t

val update_memory : t -> bytes -> unit

val load_8_s : t -> int32 -> int32

val load_8_u : t -> int32 -> int32

val store_8 : t -> addr:int32 -> int32 -> unit

val size_in_pages : t -> int32
