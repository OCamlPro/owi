type t

val zero : t

val of_i64x2 : int64 -> int64 -> t

val to_i64x2 : t -> int64 * int64

val pp : Format.formatter -> t -> unit
