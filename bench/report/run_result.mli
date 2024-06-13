type t =
  | Nothing of Rusage.t
  | Signaled of Rusage.t * int
  | Stopped of Rusage.t * int
  | Reached of Rusage.t
  | Timeout of Rusage.t
  | Other of Rusage.t * int

val is_nothing : t -> bool

val is_killed : t -> bool

val is_reached : t -> bool

val is_timeout : t -> bool

val is_other : t -> bool

val pp : Format.formatter -> t -> unit
