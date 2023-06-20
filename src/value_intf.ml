module type Iop = sig
  type num

  type const

  type vbool

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

end

module type Fop = sig
  type num

  type vbool

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
end

module type T = sig
  type vbool

  type int32

  type int64

  type float32

  type float64

  type 'a ref_value

  type 'a t =
    | I32 of int32
    | I64 of int64
    | F32 of float32
    | F64 of float64
    | Ref of 'a ref_value

  val const_i32 : Int32.t -> int32

  val const_i64 : Int64.t -> int64

  val const_f32 : Float32.t -> float32

  val const_f64 : Float64.t -> float64
  (* TODO ref *)

  val pp : Format.formatter -> 'a t -> unit

  module Bool : sig
    val not : vbool -> vbool
    val int32 : vbool -> int32
  end

  module I32 :
    Iop with type num := int32 and type vbool := vbool and type const := Int32.t

  module I64 :
    Iop with type num := int64 and type vbool := vbool and type const := Int64.t

  module F32 : Fop with type num := float32 and type vbool := vbool

  module F64 : Fop with type num := float64 and type vbool := vbool
end
