module type Iop = sig
  type num

  type const

  type vbool

  type float32

  type float64

  val zero : num

  val clz : num -> num

  val ctz : num -> num

  val popcnt : num -> num

  val add : num -> num -> num

  val sub : num -> num -> num

  val mul : num -> num -> num

  val div : num -> num -> num

  val unsigned_div : num -> num -> num

  val rem : num -> num -> num

  val unsigned_rem : num -> num -> num

  val logand : num -> num -> num

  val logor : num -> num -> num

  val logxor : num -> num -> num

  val shl : num -> num -> num

  val shr_s : num -> num -> num

  val shr_u : num -> num -> num

  val rotl : num -> num -> num

  val rotr : num -> num -> num

  val eq_const : num -> const -> vbool

  val eq : num -> num -> vbool

  val ne : num -> num -> vbool

  val lt : num -> num -> vbool

  val gt : num -> num -> vbool

  val lt_u : num -> num -> vbool

  val gt_u : num -> num -> vbool

  val le : num -> num -> vbool

  val ge : num -> num -> vbool

  val le_u : num -> num -> vbool

  val ge_u : num -> num -> vbool

  val trunc_f32_s : float32 -> num

  val trunc_f32_u : float32 -> num

  val trunc_f64_s : float64 -> num

  val trunc_f64_u : float64 -> num

  val trunc_sat_f32_s : float32 -> num

  val trunc_sat_f32_u : float32 -> num

  val trunc_sat_f64_s : float64 -> num

  val trunc_sat_f64_u : float64 -> num

  val extend_s : int -> num -> num
end

module type Fop = sig
  type num

  type vbool

  type int32

  type int64

  type same_size_int

  val zero : num

  val abs : num -> num

  val neg : num -> num

  val sqrt : num -> num

  val ceil : num -> num

  val floor : num -> num

  val trunc : num -> num

  val nearest : num -> num

  val add : num -> num -> num

  val sub : num -> num -> num

  val mul : num -> num -> num

  val div : num -> num -> num

  val min : num -> num -> num

  val max : num -> num -> num

  val copy_sign : num -> num -> num

  val eq : num -> num -> vbool

  val ne : num -> num -> vbool

  val lt : num -> num -> vbool

  val gt : num -> num -> vbool

  val le : num -> num -> vbool

  val ge : num -> num -> vbool

  val convert_i32_s : int32 -> num

  val convert_i32_u : int32 -> num

  val convert_i64_s : int64 -> num

  val convert_i64_u : int64 -> num

  val of_bits : same_size_int -> num

  val to_bits : num -> same_size_int
end

module type T = sig
  type vbool

  type int32

  type int64

  type float32

  type float64

  type ref_value

  type t =
    | I32 of int32
    | I64 of int64
    | F32 of float32
    | F64 of float64
    | Ref of ref_value

  val const_i32 : Int32.t -> int32

  val const_i64 : Int64.t -> int64

  val const_f32 : Float32.t -> float32

  val const_f64 : Float64.t -> float64
  (* TODO ref *)

  val ref_null : Simplified.heap_type -> t

  val ref_func : Func_intf.t -> t

  val ref_is_null : ref_value -> vbool

  val pp : Format.formatter -> t -> unit

  module Bool : sig
    val not : vbool -> vbool

    val or_ : vbool -> vbool -> vbool

    val and_ : vbool -> vbool -> vbool

    val int32 : vbool -> int32
  end

  module F32 : sig
    include
      Fop
        with type num := float32
         and type vbool := vbool
         and type int32 := int32
         and type int64 := int64
         and type same_size_int := int32

    val demote_f64 : float64 -> float32

    val reinterpret_i32 : int32 -> float32
  end

  module F64 : sig
    include
      Fop
        with type num := float64
         and type vbool := vbool
         and type int32 := int32
         and type int64 := int64
         and type same_size_int := int64

    val promote_f32 : float32 -> float64

    val reinterpret_i64 : int64 -> float64
  end

  module I32 : sig
    include
      Iop
        with type num := int32
         and type vbool := vbool
         and type const := Int32.t
         and type float32 := float32
         and type float64 := float64

    val to_bool : int32 -> vbool

    val reinterpret_f32 : float32 -> int32

    val wrap_i64 : int64 -> int32
  end

  module I64 : sig
    include
      Iop
        with type num := int64
         and type vbool := vbool
         and type const := Int64.t
         and type float32 := float32
         and type float64 := float64

    val of_int32 : int32 -> int64

    val to_int32 : int64 -> int32

    val reinterpret_f64 : float64 -> int64

    val extend_i32_s : int32 -> int64

    val extend_i32_u : int32 -> int64
  end
end
