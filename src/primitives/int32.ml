(* SPDX-License-Identifier: Apache-2.0 *)
(* Copyright 2017 WebAssembly Community Group participants *)
(* This file is originally from the WebAssembly reference interpreter available at https://github.com/WebAssembly/spec/tree/main/interpreter *)

(* SPDX-License-Identifier: MIT *)
(* Copyright (c) 2016--2024 Jane Street Group, LLC <opensource-contacts@janestreet.com> *)
(* The code of the `popcnt` function is originally from the `base` library available at https://github.com/janestreet/base *)

(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Modified by the Owi programmers *)

include Prelude.Int32

let clz n = of_int (Ocaml_intrinsics.Int32.count_leading_zeros n)

let ctz n = of_int (Ocaml_intrinsics.Int32.count_trailing_zeros n)

(* Taken from Base *)
let popcnt =
  let mask = 0xffff_ffffL in
  fun[@inline] x ->
    Int64.to_int32 (Int64.popcnt (Int64.logand (Int64.of_int32 x) mask))

let to_int64 n = Int64.of_int32 n [@@inline]

let eq (x : int32) y = equal x y [@@inline]

let ne (x : int32) y = compare x y <> 0 [@@inline]

let lt (x : int32) y = compare x y < 0 [@@inline]

let gt (x : int32) y = compare x y > 0 [@@inline]

let le (x : int32) y = compare x y <= 0 [@@inline]

let ge (x : int32) y = compare x y >= 0 [@@inline]

let lt_u (x : int32) y = unsigned_compare x y < 0 [@@inline]

let le_u (x : int32) y = unsigned_compare x y <= 0 [@@inline]

let gt_u (x : int32) y = unsigned_compare x y > 0 [@@inline]

let ge_u (x : int32) y = unsigned_compare x y >= 0 [@@inline]

(* In OCaml, `shift_{left,right,right_logical} are unspecified if y < 0 or y >= 32, but they're not in Wasm and thus we need to mask `y`` to only keep the low 5 bits. *)
let shl x y = shift_left x (to_int (logand y 31l)) [@@inline]

let shr_s x y = shift_right x (to_int (logand y 31l)) [@@inline]

let shr_u x y = shift_right_logical x (to_int (logand y 31l)) [@@inline]

let rotl x y =
  let n = logand y 31l in
  logor (shl x n) (shr_u x (sub 32l n))
[@@inline]

let rotr x y =
  let n = logand y 31l in
  logor (shr_u x n) (shl x (sub 32l n))
[@@inline]

let extend_s n x =
  let shift = 32 - n in
  shift_right (shift_left x shift) shift
[@@inline]

(* String conversion that allows leading signs and unsigned values *)

(* TODO: replace by Char.Ascii.digit_to_int once on 5.4 *)
let dec_digit = function
  | '0' .. '9' as c -> Char.code c - Char.code '0'
  | _ -> assert false
[@@inline]

(* TODO: replace by Char.Ascii.hex_digit_to_int once on 5.4 *)
let hex_digit = function
  | '0' .. '9' as c -> Char.code c - Char.code '0'
  | 'a' .. 'f' as c -> 0xa + Char.code c - Char.code 'a'
  | 'A' .. 'F' as c -> 0xa + Char.code c - Char.code 'A'
  | _ -> assert false
[@@inline]

let max_upper = unsigned_div minus_one 10l

let max_lower = unsigned_rem minus_one 10l

let sign_extend i =
  let sign_bit = logand (of_int (1 lsl (32 - 1))) i in
  if eq sign_bit zero then i
  else
    (* Build a sign-extension mask *)
    let sign_mask = shift_left minus_one 32 in
    logor sign_mask i

let of_string_exn s =
  let len = String.length s in

  let rec parse_hex i num =
    if i = len then num
    else
      let c = s.[i] in
      if Char.equal c '_' then parse_hex (i + 1) num
      else begin
        let digit = of_int (hex_digit c) in
        if not (le_u num (shr_u minus_one (of_int 4))) then
          Fmt.failwith "of_string (int32)"
        else parse_hex (i + 1) (logor (shift_left num 4) digit)
      end
  in

  let rec parse_dec i num =
    if i = len then num
    else
      let c = s.[i] in
      if Char.equal c '_' then parse_dec (i + 1) num
      else begin
        let digit = of_int (dec_digit c) in
        if not (lt_u num max_upper || (eq num max_upper && le_u digit max_lower))
        then Fmt.failwith "of_string (int32)"
        else parse_dec (i + 1) (add (mul num 10l) digit)
      end
  in

  let parse_int i =
    if not (len - i > 0) then Fmt.failwith "of_string (int32)"
    else if i + 2 <= len && Char.equal s.[i] '0' && Char.equal s.[i + 1] 'x'
    then parse_hex (i + 2) zero
    else parse_dec i zero
  in

  let parsed =
    match s.[0] with
    | '+' -> parse_int 1
    | '-' ->
      let n = parse_int 1 in
      if not (ge (sub n one) minus_one) then Fmt.failwith "of_string (int32)"
      else neg n
    | _ -> parse_int 0
  in

  sign_extend parsed

let of_string s = try Some (of_string_exn s) with _ -> None
