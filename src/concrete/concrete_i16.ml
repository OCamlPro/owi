(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

type t = int

let of_i8x2 a b = a land 0xFF lor ((b land 0xFF) lsl 8)

let wrap x = x land 0xFFFF

let wrap_i32 x = Int32.logand x 0xFFFFl |> Int32.to_int

let to_int_s x =
  let x = x land 0xFFFF in
  if x land 0x8000 <> 0 then x - 0x10000 else x

let to_int_u x = x land 0xFFFF

let to_i32_s x =
  if x land 0x8000 <> 0 then Int32.of_int (x lor 0xffff_0000)
  else Int32.of_int x

let to_i32_u x = Int32.of_int (x land 0xffff)

let of_int x = wrap x

let add x y = wrap (x + y)

let sub x y = wrap (x - y)

let mul x y = wrap (to_int_s x * to_int_s y)

let neg x = wrap (-to_int_s x)

let abs x = wrap (abs (to_int_s x))

let popcnt x =
  let rec loop n acc =
    if n = 0 then acc else loop (n lsr 1) (acc + (n land 1))
  in
  loop (to_int_u x) 0 |> wrap

let eq x y = x = y

let ne x y = x <> y

let lt_s x y = to_int_s x < to_int_s y

let lt_u x y = to_int_u x < to_int_u y

let le_s x y = to_int_s x <= to_int_s y

let le_u x y = to_int_u x <= to_int_u y

let gt_s x y = to_int_s x > to_int_s y

let gt_u x y = to_int_u x > to_int_u y

let ge_s x y = to_int_s x >= to_int_s y

let ge_u x y = to_int_u x >= to_int_u y

let shl x y = wrap (to_int_u x lsl (y land 15))

let shr_s x y = wrap (to_int_s x asr (y land 15))

let shr_u x y = wrap (to_int_u x lsr (y land 15))

let min_s x y = if lt_s x y then x else y

let min_u x y = if lt_u x y then x else y

let max_s x y = if gt_s x y then x else y

let max_u x y = if gt_u x y then x else y

let add_sat_s x y =
  let v = to_int_s x + to_int_s y in
  if v > 32767 then 0x7FFF else if v < -32768 then 0x8000 else wrap v

let add_sat_u x y =
  let v = to_int_u x + to_int_u y in
  if v > 65535 then 65535 else v

let sub_sat_s x y =
  let v = to_int_s x - to_int_s y in
  if v > 32767 then 0x7FFF else if v < -32768 then 0x8000 else wrap v

let sub_sat_u x y =
  let v = to_int_u x - to_int_u y in
  if v < 0 then 0 else v

let q15mulr_sat_s x y =
  let v = ((to_int_s x * to_int_s y) + 0x4000) asr 15 in
  if v > 32767 then 0x7FFF else if v < -32768 then 0x8000 else wrap v

let avgr_u x y = (to_int_u x + to_int_u y + 1) / 2

let narrow_i8_s x =
  let v = to_int_s x in
  if v > 127 then 0x7F else if v < -128 then 0x80 else v land 0xFF

let narrow_i8_u x =
  let v = to_int_s x in
  if v < 0 then 0 else if v > 255 then 255 else v

let to_i8x2 x = (x land 0xFF, (x lsr 8) land 0xFF)
