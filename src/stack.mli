type 'a t = 'a Value.t list

val empty : 'a t

val pp : Format.formatter -> 'a t -> unit

(** pop operations *)

val drop : 'a t -> 'a t

val pop : 'a t -> 'a Value.t * 'a t

val pop_n : 'a t -> int -> 'a t * 'a t

val keep : 'a t -> int -> 'a t

val pop_bool : 'a t -> bool * 'a t

val pop_i32 : 'a t -> int32 * 'a t

val pop_i32_to_char : 'a t -> char * 'a t

val pop_i32_to_int : 'a t -> int * 'a t

val pop2_i32 : 'a t -> (int32 * int32) * 'a t

val pop_i64 : 'a t -> int64 * 'a t

val pop2_i64 : 'a t -> (int64 * int64) * 'a t

val pop_f32 : 'a t -> Float32.t * 'a t

val pop2_f32 : 'a t -> (Float32.t * Float32.t) * 'a t

val pop_f64 : 'a t -> Float64.t * 'a t

val pop2_f64 : 'a t -> (Float64.t * Float64.t) * 'a t

val pop_ref : 'a t -> 'a Value.t * 'a t

val pop_is_null : 'a t -> bool * 'a t

val pop_as_ref : 'a t -> 'a Value.ref_value * 'a t

val pop_as_externref : 'a Value.Extern_ref.ty -> 'b t -> 'a * 'b t

(** push operations *)

val push : 'a t -> 'a Value.t -> 'a t

val push_bool : 'a t -> bool -> 'a t

val push_i32 : 'a t -> int32 -> 'a t

val push_i32_of_int : 'a t -> int -> 'a t

val push_i64 : 'a t -> int64 -> 'a t

val push_i64_of_int : 'a t -> int -> 'a t

val push_f32 : 'a t -> Float32.t -> 'a t

val push_f64 : 'a t -> Float64.t -> 'a t

val push_as_externref : 'a t -> 'b Value.Extern_ref.ty -> 'b -> 'a t
