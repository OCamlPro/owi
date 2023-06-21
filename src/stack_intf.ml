module type Value = Value_intf.T

module type S = sig
  type vbool

  type int32

  type int64

  type float32

  type float64

  type 'a ref_value

  type 'a value

  type 'a t = 'a value list

  val empty : 'a t

  val pp : Format.formatter -> 'a t -> unit

  (** pop operations *)

  val drop : 'a t -> 'a t

  val drop_n : 'a list -> int -> 'a list

  val pop : 'a t -> 'a value * 'a t

  val pop_n : 'a t -> int -> 'a t * 'a t

  val keep : 'a t -> int -> 'a t

  val pop_bool : 'a t -> vbool * 'a t

  val pop_i32 : 'a t -> int32 * 'a t

  (* val pop_i32_to_char : 'a t -> char * 'a t *)

  (* val pop_i32_to_int : 'a t -> int * 'a t *)

  (* val pop_ui32_to_int : 'a t -> int * 'a t *)

  val pop2_i32 : 'a t -> (int32 * int32) * 'a t

  val pop_i64 : 'a t -> int64 * 'a t

  val pop2_i64 : 'a t -> (int64 * int64) * 'a t

  val pop_f32 : 'a t -> float32 * 'a t

  val pop2_f32 : 'a t -> (float32 * float32) * 'a t

  val pop_f64 : 'a t -> float64 * 'a t

  val pop2_f64 : 'a t -> (float64 * float64) * 'a t

  val pop_ref : 'a t -> 'a value * 'a t

  (* val pop_is_null : 'a t -> bool * 'a t *)

  val pop_as_ref : 'a t -> 'a ref_value * 'a t

  (* val pop_as_externref : 'a Type_id.ty -> 'b t -> 'a * 'b t *)

  (** push operations *)

  val push : 'a t -> 'a value -> 'a t

  val push_const_bool : 'a t -> Stdlib.Bool.t -> 'a t

  val push_bool : 'a t -> vbool -> 'a t

  val push_i32 : 'a t -> int32 -> 'a t

  val push_const_i32 : 'a t -> Int32.t -> 'a t

  val push_i32_of_int : 'a t -> int -> 'a t

  val push_i64 : 'a t -> int64 -> 'a t

  val push_const_i64 : 'a t -> Int64.t -> 'a t

  val push_i64_of_int : 'a t -> int -> 'a t

  val push_f32 : 'a t -> float32 -> 'a t

  val push_const_f32 : 'a t -> Float32.t -> 'a t

  val push_f64 : 'a t -> float64 -> 'a t

  val push_const_f64 : 'a t -> Float64.t -> 'a t

  val push_as_externref : 'a t -> 'b Type_id.ty -> 'b -> 'a t

  val push_array : 'a t -> unit Array.t -> 'a t
end
