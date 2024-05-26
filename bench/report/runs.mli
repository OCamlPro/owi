type t = Run.t list

val count_nothing : t -> int

val count_reached : t -> int

val count_timeout : t -> int

val count_other : t -> int

val count_killed : t -> int

val min_clock : t -> float

val max_clock : t -> float

val to_distribution : max_time:int -> t -> float list

val pp_quick_results : Format.formatter -> t -> unit
