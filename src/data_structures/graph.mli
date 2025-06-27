type t

val init : (int * string option * int list) list -> int list -> t

val pp_dot : Format.formatter -> t -> unit

val get_info : t -> string option
