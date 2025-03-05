type t =
  { i : int
  ; res : Run_result.t
  ; file : Fpath.t
  }

val clock : t -> float

val utime : t -> float

val stime : t -> float

val maxrss : t -> int64

val is_nothing : t -> bool

val is_killed : t -> bool

val is_reached : t -> bool

val is_timeout : t -> bool

val is_other : t -> bool

val pp_header : int -> Format.formatter -> int * Fpath.t -> unit
