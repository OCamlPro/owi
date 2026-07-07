(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

include Int32

type t = int32

let wrap_i64 x = Int64.to_int32 x

let trunc_f32_s x =
  if Float32.ne x x then Error `Conversion_to_integer
  else
    let xf = Float32.to_float x in
    if
      let xf = Float64.of_float xf in
      let mif = Int32.(to_float min_int) in
      Float64.(le (of_float ~-.mif) xf) || Float64.(lt xf (of_float mif))
    then Error `Integer_overflow
    else Ok (Int32.of_float xf)

let trunc_f32_u x =
  if Float32.ne x x then Error `Conversion_to_integer
  else
    let xf = Float32.to_float x in
    if
      let xf = Float64.of_float xf in
      Float64.(le (of_float @@ (-.Int32.(to_float min_int) *. 2.0)) xf)
      || Float64.(le xf (Float64.of_float ~-.1.0))
    then Error `Integer_overflow
    else Ok Int64.(to_int32 (of_float xf))

let trunc_f64_s x =
  if Float64.ne x x then Error `Conversion_to_integer
  else if
    let mif = Int32.(to_float min_int) in
    Float64.(le (of_float @@ -.mif) x)
    || Float64.(le x (of_float @@ (mif -. 1.0)))
  then Error `Integer_overflow
  else Ok (Int32.of_float (Float64.to_float x))

let trunc_f64_u x =
  if Float64.ne x x then Error `Conversion_to_integer
  else if
    let mif = Int32.to_float Int32.min_int in
    Float64.(le (of_float @@ (-.mif *. 2.0)) x)
    || Float64.(le x (of_float ~-.1.0))
  then Error `Integer_overflow
  else Ok Int64.(to_int32 (of_float (Float64.to_float x)))

let trunc_sat_f32_s x =
  if Float32.ne x x then 0l
  else
    let xf = Float32.to_float x |> Float64.of_float in
    let mif = Int32.(to_float min_int) in
    if Float64.(lt xf (of_float mif)) then Int32.min_int
    else if Float64.(le (of_float ~-.mif) xf) then Int32.max_int
    else Int32.of_float (Float64.to_float xf)

let trunc_sat_f32_u x =
  if Float32.ne x x then 0l
  else
    let xf = Float32.to_float x |> Float64.of_float in
    if Float64.(le xf (of_float ~-.1.0)) then 0l
    else if Float64.(le (of_float (~-.Int32.(to_float min_int) *. 2.0)) xf) then
      -1l
    else Int64.(to_int32 @@ of_float (Float64.to_float xf))

let trunc_sat_f64_s x =
  if Float64.ne x x then 0l
  else if Float64.(le x @@ of_float @@ Int32.(to_float min_int)) then
    Int32.min_int
  else if Float64.(le (of_float ~-.Int32.(to_float min_int)) x) then
    Int32.max_int
  else Int32.of_float @@ Float64.to_float x

let trunc_sat_f64_u x =
  if Float64.ne x x then 0l
  else if Float64.(le x @@ of_float ~-.1.0) then 0l
  else if Float64.(le (of_float ~-.(Int32.(to_float min_int) *. 2.0)) x) then
    -1l
  else Int64.(to_int32 (of_float @@ Float64.to_float x))

let reinterpret_f32 = Float32.to_bits

let to_boolean = function 0l -> false | _i -> true

let of_boolean = function false -> 0l | true -> 1l

let of_concrete (v : t) : t = v

let eq_concrete (v1 : t) (v2 : Int32.t) = eq v1 v2

let pp = Fmt.int32

let of_int32 (v : int32) : t = v

let to_int32 (v : t) : int32 = v

let min_int = Int32.min_int

let eqz (v : t) = eq v zero

let ( = ) = eq

let ( + ) = add

let of_i16x2 a b =
  let lower = logand (of_int a) 0xFFFFl in
  let upper = shl (of_int b) 16l in
  logor lower upper

let of_i8x4 a b c d =
  of_i16x2 (Concrete_i16.of_i8x2 a b) (Concrete_i16.of_i8x2 c d)

let narrow_i16_s x =
  if lt x (-32768l) then -32768 else if lt 32767l x then 32767 else to_int x

let narrow_i16_u x =
  let x = logand x 0xffff_ffffl in
  if lt x 0l then 0 else if lt 65535l x then 65535 else to_int x

let neg x = sub 0l x

let abs x = if lt x 0l then neg x else x

let min_s x y = if lt x y then x else y

let min_u x y = if lt (logxor x min_int) (logxor y min_int) then x else y

let max_s x y = if lt x y then y else x

let max_u x y = if lt (logxor x min_int) (logxor y min_int) then y else x
