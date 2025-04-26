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
  fun [@inline] x ->
    Int64.to_int32 (Int64.popcnt (Int64.logand (Int64.of_int32 x) mask))

let of_int64 n = Int64.to_int32 n [@@inline]

let to_int64 n = Int64.of_int32 n [@@inline]

(* Unsigned comparison in terms of signed comparison. *)
let cmp_u x op y = op (add x min_int) (add y min_int) [@@inline]

let eq (x : int32) y = equal x y [@@inline]

let ne (x : int32) y = compare x y <> 0 [@@inline]

let lt (x : int32) y = compare x y < 0 [@@inline]

let gt (x : int32) y = compare x y > 0 [@@inline]

let le (x : int32) y = compare x y <= 0 [@@inline]

let ge (x : int32) y = compare x y >= 0 [@@inline]

let lt_u x y = cmp_u x lt y [@@inline]

let le_u x y = cmp_u x le y [@@inline]

let gt_u x y = cmp_u x gt y [@@inline]

let ge_u x y = cmp_u x ge y [@@inline]

(* If bit (32 - 1) is set, sx will sign-extend t to maintain the
 * invariant that small ints are stored sign-extended inside a wider int. *)
let sx x =
  Int64.to_int32
  @@ Int64.shift_right (Int64.shift_left (Int64.of_int32 x) 32) 32

(* We don't override min_int and max_int since those are used
 * by other functions (like parsing), and rely on it being
 * min/max for int32 *)
(* The smallest signed |32|-bits int. *)
let low_int = shift_left minus_one 31

(* The largest signed |32|-bits int. *)
let high_int = logxor low_int minus_one

(* WebAssembly's shifts mask the shift count according to the 32. *)
let shift f x y = f x (to_int (logand y 31l))

let shl x y = sx (shift shift_left x y)

let shr_s x y = shift shift_right x y

let shr_u x y = sx (shift shift_right_logical x y)

let rotl x y =
  let n = logand y 31l in
  logor (shl x n) (shr_u x (sub 32l n))

let rotr x y =
  let n = logand y 31l in
  logor (shr_u x n) (shl x (sub 32l n))

let extend_s n x =
  let shift = 32 - n in
  shift_right (shift_left x shift) shift

(* String conversion that allows leading signs and unsigned values *)

let require b = if not b then Fmt.failwith "of_string (int32)" [@@inline]

let dec_digit = function
  | '0' .. '9' as c -> Char.code c - Char.code '0'
  | _ -> Fmt.failwith "of_string"
[@@inline]

let hex_digit = function
  | '0' .. '9' as c -> Char.code c - Char.code '0'
  | 'a' .. 'f' as c -> 0xa + Char.code c - Char.code 'a'
  | 'A' .. 'F' as c -> 0xa + Char.code c - Char.code 'A'
  | _ -> Fmt.failwith "of_string"
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
        require (le_u num (shr_u minus_one (of_int 4)));
        parse_hex (i + 1) (logor (shift_left num 4) digit)
      end
  in

  let rec parse_dec i num =
    if i = len then num
    else
      let c = s.[i] in
      if Char.equal c '_' then parse_dec (i + 1) num
      else begin
        let digit = of_int (dec_digit c) in
        require
          (lt_u num max_upper || (eq num max_upper && le_u digit max_lower));
        parse_dec (i + 1) (add (mul num 10l) digit)
      end
  in

  let parse_int i =
    require (len - i > 0);
    if i + 2 <= len && Char.equal s.[i] '0' && Char.equal s.[i + 1] 'x' then
      parse_hex (i + 2) zero
    else parse_dec i zero
  in

  require (len > 0);

  let parsed =
    match s.[0] with
    | '+' -> parse_int 1
    | '-' ->
      let n = parse_int 1 in
      require (ge (sub n one) minus_one);
      neg n
    | _ -> parse_int 0
  in

  let parsed = sign_extend parsed in
  require (le low_int parsed && le parsed high_int);
  parsed

let of_string s = try Some (of_string_exn s) with _ -> None
