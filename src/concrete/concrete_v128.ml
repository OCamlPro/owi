type t =
  | F32x4 of Float32.t * Float32.t * Float32.t * Float32.t
  | F64x2 of Float64.t * Float64.t
  | I32x4 of int32 * int32 * int32 * int32
  | I64x2 of int64 * int64

let i16_of_i8x2 a b =
  let upper = a lsl 8 in
  upper lor (b land 0xFF)

let i32_of_i16x2 a b =
  let upper = Int32.shl (Int32.of_int b) 16l in
  let lower = Int32.of_int a in
  Int32.logor upper (Int32.logand lower 0xFFFFl)

let i64_of_i32x2 a b =
  let upper = Int64.shl (Int32.to_int64 b) 32L in
  let lower = Int32.to_int64 a in
  Int64.logor upper (Int64.logand lower 0xFFFFFFFFL)

let i64_of_i16x4 a b c d = i64_of_i32x2 (i32_of_i16x2 a b) (i32_of_i16x2 c d)

let i32_of_i8x4 a b c d = i32_of_i16x2 (i16_of_i8x2 a b) (i16_of_i8x2 c d)

let i64_of_i8x8 a b c d e f g h =
  i64_of_i32x2 (i32_of_i8x4 a b c d) (i32_of_i8x4 e f g h)

let of_i8x16 a b c d e f g h i j k l m n o p =
  I64x2 (i64_of_i8x8 a b c d e f g h, i64_of_i8x8 i j k l m n o p)

let of_i16x8 a b c d e f g h = I64x2 (i64_of_i16x4 a b c d, i64_of_i16x4 e f g h)

let of_i32x4 a b c d = I32x4 (a, b, c, d)

let of_i64x2 a b = I64x2 (a, b)

let of_f32x4 a b c d = F32x4 (a, b, c, d)

let of_f64x2 a b = F64x2 (a, b)

let to_i64x2 t =
  match t with
  | I64x2 (a, b) -> (a, b)
  | I32x4 (a, b, c, d) -> (i64_of_i32x2 a b, i64_of_i32x2 c d)
  | F64x2 (a, b) -> (Float64.to_bits a, Float64.to_bits b)
  | F32x4 (a, b, c, d) ->
    let a = Float32.to_bits a in
    let b = Float32.to_bits b in
    let c = Float32.to_bits c in
    let d = Float32.to_bits d in
    (i64_of_i32x2 a b, i64_of_i32x2 c d)

let i64_to_i32x2 a =
  let high = Int64.(to_int32 (shift_right_logical a 32)) in
  let low = Int64.to_int32 a in
  (low, high)

let i64x2_to_i32x4 (v1, v2) =
  let a, b = i64_to_i32x2 v1 in
  let c, d = i64_to_i32x2 v2 in
  (a, b, c, d)

let to_i32x4 = function
  | I32x4 (a, b, c, d) -> (a, b, c, d)
  | v -> i64x2_to_i32x4 (to_i64x2 v)

let zero = of_i64x2 0L 0L

let eq a b =
  let a1, a2 = to_i64x2 a in
  let b1, b2 = to_i64x2 b in
  Int64.eq a1 b1 && Int64.eq a2 b2

let pp ppf v =
  match v with
  | I64x2 (a, b) -> Fmt.pf ppf "i64x2 %Ld %Ld" a b
  | I32x4 (a, b, c, d) -> Fmt.pf ppf "i32x4 %ld %ld %ld %ld" a b c d
  | F64x2 (a, b) -> Fmt.pf ppf "f64x2 %a %a" Float64.pp a Float64.pp b
  | F32x4 (a, b, c, d) ->
    Fmt.pf ppf "f32x4 %a %a %a %a" Float32.pp a Float32.pp b Float32.pp c
      Float32.pp d

let of_concrete v = v
