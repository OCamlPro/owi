(** Custom Int32 module for Wasm. *)

type t = int32

exception Overflow

val min_int : t

val max_int : t

val zero : t

(** conversion *)

val bits_of_float : float -> t

val float_of_bits : t -> float

val of_float : float -> t

val to_float : t -> float

val of_string : string -> t

val of_int : int -> t

val to_int : t -> int

val of_int64 : int64 -> t

val to_int64 : t -> int64

val extend_s : int -> t -> t

val unsigned_to_int : t -> int option

(** unary operators *)

val clz : t -> int

val ctz : t -> int

val popcnt : t -> int

val lognot : t -> t

(** comparison operators *)

val le_u : t -> t -> bool

val lt_u : t -> t -> bool

val ge_u : t -> t -> bool

val gt_u : t -> t -> bool

(** binary operators *)

val logor : t -> t -> t

val logand : t -> t -> t

val logxor : t -> t -> t

val rotl : t -> t -> t

val rotr : t -> t -> t

val shl : t -> t -> t

val shr_s : t -> t -> t

val shr_u : t -> t -> t

val shift_right_logical : t -> int -> t

val add : t -> t -> t

val sub : t -> t -> t

val mul : t -> t -> t

val div : t -> t -> t

val unsigned_div : t -> t -> t

val rem : t -> t -> t

val unsigned_rem : t -> t -> t
