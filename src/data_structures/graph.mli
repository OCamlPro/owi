type t

val empty : t

val set_entry_points : t -> int list -> t

val add_node : t -> int -> string option -> int list -> t

val pp_dot : Format.formatter -> t -> unit

val find_indice : t -> string -> int option
