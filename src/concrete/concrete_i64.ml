(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

include Int64

type t = int64

let extend_i32_s x = Int64.of_int32 x

let extend_i32_u x = Int64.logand (Int64.of_int32 x) 0x0000_0000_ffff_ffffL

let trunc_f32_s x =
  if Float32.ne x x then Error `Conversion_to_integer
  else if
    let mif = Int64.(to_float min_int) in
    Float32.(le (of_float ~-.mif) x) || Float32.(lt x @@ of_float mif)
  then Error `Integer_overflow
  else Ok (Int64.of_float @@ Float32.to_float x)

let trunc_f32_u x =
  let mif = Int64.(to_float min_int) in
  if Float32.ne x x then Error `Conversion_to_integer
  else if
    Float32.(le (of_float ~-.(mif *. 2.0)) x)
    || Float32.(le x @@ of_float ~-.1.0)
  then Error `Integer_overflow
  else if Float32.(le (of_float ~-.mif) x) then
    Ok Int64.(logxor (of_float (Float32.to_float x -. 0x1p63)) min_int)
  else Ok (Int64.of_float @@ Float32.to_float x)

let trunc_f64_s x =
  if Float64.ne x x then Error `Conversion_to_integer
  else if
    let mif = Int64.(to_float min_int) in
    Float64.(le (of_float ~-.mif) x) || Float64.(lt x @@ of_float mif)
  then Error `Integer_overflow
  else Ok (Int64.of_float @@ Float64.to_float x)

let trunc_f64_u x =
  let mif = Int64.(to_float min_int) in
  if Float64.ne x x then Error `Conversion_to_integer
  else if
    Float64.(le (of_float (~-.mif *. 2.0)) x)
    || Float64.(le x @@ of_float ~-.1.0)
  then Error `Integer_overflow
  else if Float64.(le (of_float ~-.mif) x) then
    Ok Int64.(logxor (of_float (Float64.to_float x -. 0x1p63)) min_int)
  else Ok (Int64.of_float @@ Float64.to_float x)

let trunc_sat_f32_s x =
  if Float32.ne x x then 0L
  else
    let mif = Int64.(to_float min_int) in
    if Float32.(lt x @@ of_float mif) then Int64.min_int
    else if Float32.(le (of_float ~-.mif) x) then Int64.max_int
    else Int64.of_float (Float32.to_float x)

let trunc_sat_f32_u x =
  if Float32.ne x x then 0L
  else
    let mif = Int64.(to_float min_int) in
    if Float32.(le x @@ of_float ~-.1.0) then 0L
    else if Float32.(le (of_float (~-.mif *. 2.0)) x) then -1L
    else if Float32.(le (of_float ~-.mif) x) then
      Int64.(
        logxor (of_float (Float32.to_float x -. 9223372036854775808.0)) min_int )
    else Int64.of_float @@ Float32.to_float x

let trunc_sat_f64_s x =
  if Float64.ne x x then 0L
  else
    let mif = Int64.(to_float min_int) in
    if Float64.(lt x @@ of_float mif) then Int64.min_int
    else if Float64.(le (of_float ~-.mif) x) then Int64.max_int
    else Int64.of_float @@ Float64.to_float x

let trunc_sat_f64_u x =
  if Float64.ne x x then 0L
  else
    let mif = Int64.(to_float min_int) in
    if Float64.(le x @@ of_float ~-.1.0) then 0L
    else if Float64.(le (of_float @@ (~-.mif *. 2.0)) x) then -1L
    else if Float64.(le (of_float ~-.mif) x) then
      Int64.(
        logxor (of_float (Float64.to_float x -. 9223372036854775808.0)) min_int )
    else Int64.of_float @@ Float64.to_float x

let reinterpret_f64 = Float64.to_bits

let of_concrete v = v

let eq_concrete (v1 : t) (v2 : Int64.t) = eq v1 v2

let pp = Fmt.int64

let of_int64 (v : int64) : t = v

let to_int64 (v : t) : int64 = v

let min_int = Int64.min_int

let eqz (v : t) = eq v zero

let ( = ) = eq

let ( + ) = add

let ( * ) = mul

let ( / ) = div

let of_i32x2 a b =
  let lower = logand (Concrete_i32.to_int64 a) 0xFFFFFFFFL in
  let upper = shl (Concrete_i32.to_int64 b) 32L in
  logor lower upper

let of_i16x4 a b c d =
  of_i32x2 (Concrete_i32.of_i16x2 a b) (Concrete_i32.of_i16x2 c d)

let of_i8x8 a b c d e f g h =
  of_i32x2 (Concrete_i32.of_i8x4 a b c d) (Concrete_i32.of_i8x4 e f g h)

let to_i32x2 a =
  let low = to_int32 a in
  let high = to_int32 (shift_right_logical a 32) in
  (low, high)

let neg x = sub 0L x
