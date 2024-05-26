type t =
  | Nothing of Rusage.t
  | Killed of Rusage.t
  | Reached of Rusage.t
  | Timeout of Rusage.t
  | Other of int * Rusage.t

val is_nothing : t -> bool

val is_killed : t -> bool

val is_reached : t -> bool

val is_timeout : t -> bool

val is_other : t -> bool

val pp : Format.formatter -> t -> unit
