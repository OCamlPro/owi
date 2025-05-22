type t

val zero : t

val of_i8x16 :
  int -> int -> int -> int -> int -> int -> int -> int ->
  int -> int -> int -> int -> int -> int -> int -> int ->
  t

val of_i16x8 : int -> int -> int -> int -> int -> int -> int -> int -> t

val of_i32x4 : int32 -> int32 -> int32 -> int32 -> t

val of_i64x2 : int64 -> int64 -> t

val of_f32x4 : Float32.t -> Float32.t -> Float32.t -> Float32.t -> t

val of_f64x2 : Float64.t -> Float64.t -> t

val to_i64x2 : t -> int64 * int64

val to_i32x4 : t -> int32 * int32 * int32 * int32

val eq : t -> t -> bool

val pp : Format.formatter -> t -> unit
