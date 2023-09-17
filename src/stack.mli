(** runtime stack module *)

type t = Value.t list

val empty : t

val pp : Format.formatter -> t -> unit

(** pop operations *)

val drop : t -> t

val drop_n : 'a list -> int -> 'a list

val pop : t -> Value.t * t

val pop_n : t -> int -> t * t

val keep : t -> int -> t

val pop_bool : t -> bool * t

val pop_i32 : t -> int32 * t

val pop_i32_to_char : t -> char * t

val pop_i32_to_int : t -> int * t

val pop_ui32_to_int : t -> int * t

val pop2_i32 : t -> (int32 * int32) * t

val pop_i64 : t -> int64 * t

val pop2_i64 : t -> (int64 * int64) * t

val pop_f32 : t -> Float32.t * t

val pop2_f32 : t -> (Float32.t * Float32.t) * t

val pop_f64 : t -> Float64.t * t

val pop2_f64 : t -> (Float64.t * Float64.t) * t

val pop_ref : t -> Value.t * t

val pop_is_null : t -> bool * t

val pop_as_ref : t -> Value.ref_value * t

val pop_as_externref : 'a Type.Id.t -> t -> 'a * t

(** push operations *)

val push : t -> Value.t -> t

val push_bool : t -> bool -> t

val push_i32 : t -> int32 -> t

val push_i32_of_int : t -> int -> t

val push_i64 : t -> int64 -> t

val push_i64_of_int : t -> int -> t

val push_f32 : t -> Float32.t -> t

val push_f64 : t -> Float64.t -> t

val push_as_externref : t -> 'b Type.Id.t -> 'b -> t

val push_array : t -> unit Array.t -> t
