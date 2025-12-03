include Float64

type t = Float64.t

let promote_f32 x =
  if Float32.eq x x then Float64.of_float @@ Float32.to_float x
  else
    let nan32bits = Concrete_i64.extend_i32_u (Float32.to_bits x) in
    let sign_field = Int64.(shift_left (shift_right_logical nan32bits 31) 63) in
    let significand_field =
      Int64.(shift_right_logical (shift_left nan32bits 41) 12)
    in
    let fields = Int64.logor sign_field significand_field in
    let nan64bits = Int64.logor 0x7ff8_0000_0000_0000L fields in
    Float64.of_bits nan64bits

let convert_i32_s x = Float64.of_float (Int32.to_float x)

(*
 * Unlike the other convert_u functions, the high half of the i32 range is
 * within the range where f32 can represent odd numbers, so we can't do the
 * shift. Instead, we can use int64 signed arithmetic.
 *)
let convert_i32_u x =
  Float64.of_float Int64.(to_float (logand (of_int32 x) 0x0000_0000_ffff_ffffL))

let convert_i64_s x = Float64.of_float (Int64.to_float x)

(*
 * Values in the low half of the int64 range can be converted with a signed
 * conversion. The high half is beyond the range where f64 can represent odd
 * numbers, so we can shift the value right, adjust the least significant
 * bit to round correctly, do a conversion, and then scale it back up.
 *)
let convert_i64_u x =
  Float64.of_float
    Int64.(
      if Int64.ge x zero then to_float x
      else to_float (logor (shift_right_logical x 1) (logand x 1L)) *. 2.0 )

let reinterpret_i64 = Float64.of_bits

let of_concrete v = v
