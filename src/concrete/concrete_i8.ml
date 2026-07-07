(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

type t = int

let wrap x = x land 0xFF

let wrap_i32 x = Int32.logand x 0xFFl |> Int32.to_int

let to_int_s x =
  let x = x land 0xFF in
  if x land 0x80 <> 0 then x - 0x100 else x

let to_int_u x = x land 0xFF

let add x y = wrap (x + y)

let sub x y = wrap (x - y)

let neg x = wrap (-to_int_s x)

let abs x = wrap (abs (to_int_s x))

let popcnt x =
  let rec loop n acc =
    if n = 0 then acc else loop (n lsr 1) (acc + (n land 1))
  in
  loop (to_int_u x) 0

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

let shl x y = wrap (to_int_u x lsl (y land 7))

let shr_s x y = wrap (to_int_s x asr (y land 7))

let shr_u x y = wrap (to_int_u x lsr (y land 7))

let min_s x y = if lt_s x y then x else y

let min_u x y = if lt_u x y then x else y

let max_s x y = if gt_s x y then x else y

let max_u x y = if gt_u x y then x else y

let add_sat_s x y =
  let v = to_int_s x + to_int_s y in
  if v > 127 then 127 else if v < -128 then 128 else wrap v

let add_sat_u x y =
  let v = to_int_u x + to_int_u y in
  if v > 255 then 255 else v

let sub_sat_s x y =
  let v = to_int_s x - to_int_s y in
  if v > 127 then 127 else if v < -128 then 128 else wrap v

let sub_sat_u x y =
  let v = to_int_u x - to_int_u y in
  if v < 0 then 0 else v

let avgr_u x y = (to_int_u x + to_int_u y + 1) / 2
