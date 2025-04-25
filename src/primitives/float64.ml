(* SPDX-License-Identifier: Apache-2.0 *)
(* Copyright 2017 WebAssembly Community Group participants *)
(* This file is originally from the WebAssembly reference interpreter available at https://github.com/WebAssembly/spec/tree/main/interpreter *)

(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Modified by the Owi programmers *)

let pos_nan = 0x7ff8_0000_0000_0000L

let neg_nan = 0xfff8_0000_0000_0000L

let bare_nan = 0x7ff0_0000_0000_0000L

let to_hex_string = Fmt.str "%Lx"

type t = Int64.t

let pos_inf = Int64.bits_of_float (1.0 /. 0.0)

let neg_inf = Int64.bits_of_float (-.(1.0 /. 0.0))

let of_float = Int64.bits_of_float

let to_float = Int64.float_of_bits

let of_bits x = x

let to_bits x = x

let is_inf x = Int64.eq x pos_inf || Int64.eq x neg_inf

let is_nan x =
  let xf = Int64.float_of_bits x in
  Float.is_nan xf

let is_pos_nan f = Int64.eq f pos_nan

let is_neg_nan f = Int64.eq f neg_nan

(*
 * When the result of an arithmetic operation is NaN, the most significant
 * bit of the significand field is set.
 *)
let canonicalize_nan x = Int64.logor x pos_nan

(*
 * When the result of a binary operation is NaN, the resulting NaN is computed
 * from one of the NaN inputs, if there is one. If both are NaN, one is
 * selected nondeterminstically. If neither, we use a default NaN value.
 *)
let determine_binary_nan x y =
  (*
   * TODO: There are two nondeterministic things we could do here. When both
   * x and y are NaN, we can nondeterministically pick which to return. And
   * when neither is NaN, we can nondeterministically pick whether to return
   * pos_nan or neg_nan.
   *)
  let nan = if is_nan x then x else if is_nan y then y else pos_nan in
  canonicalize_nan nan

(*
 * When the result of a unary operation is NaN, the resulting NaN is computed
 * from one of the NaN input, if there it is NaN. Otherwise, we use a default
 * NaN value.
 *)
let determine_unary_nan x =
  (*
   * TODO: There is one nondeterministic thing we could do here. When the
   * operand is not NaN, we can nondeterministically pick whether to return
   * pos_nan or neg_nan.
   *)
  let nan = if is_nan x then x else pos_nan in
  canonicalize_nan nan

let binary x op y =
  let xf = to_float x in
  let yf = to_float y in
  let t = op xf yf in
  if not @@ Float.is_nan t then of_float t else determine_binary_nan x y

let unary op x =
  let t = op (to_float x) in
  if not @@ Float.is_nan t then of_float t else determine_unary_nan x

let zero = of_float 0.0

let add x y = binary x ( +. ) y

let sub x y = binary x ( -. ) y

let mul x y = binary x ( *. ) y

let div x y = binary x ( /. ) y

let sqrt x = unary Float.sqrt x

let ceil x = unary Float.ceil x

let floor x = unary Float.floor x

let trunc x =
  let xf = to_float x in
  (* preserve the sign of zero *)
  if Float.equal xf 0.0 then x
  else
    (* trunc is either ceil or floor depending on which one is toward zero *)
    let f =
      if Float.compare xf 0.0 < 0 then Float.ceil xf else Float.floor xf
    in
    let result = of_float f in
    if is_nan result then determine_unary_nan result else result

let nearest x =
  let xf = to_float x in
  (* preserve the sign of zero *)
  if Float.compare xf 0.0 = 0 then x
  else
    (* nearest is either ceil or floor depending on which is nearest or even *)
    let u = Float.ceil xf in
    let d = Float.floor xf in
    let um = abs_float (xf -. u) in
    let dm = abs_float (xf -. d) in
    let u_or_d =
      Float.compare um dm < 0
      || Float.compare um dm = 0
         &&
         let h = u /. 2. in
         Float.compare (Float.floor h) h = 0
    in
    let f = if u_or_d then u else d in
    let result = of_float f in
    if is_nan result then determine_unary_nan result else result

let min x y =
  let xf = to_float x in
  let yf = to_float y in

  if Float.is_nan xf || Float.is_nan yf then determine_binary_nan x y
  else
    let delta = Float.compare xf yf in
    if delta < 0 then x
    else if delta > 0 then y
    else (* min -0 0 is -0 *)
      Int64.logor x y

let max x y =
  let xf = to_float x in
  let yf = to_float y in

  if Float.is_nan xf || Float.is_nan yf then determine_binary_nan x y
  else
    let delta = Float.compare xf yf in
    if delta > 0 then x
    else if delta < 0 then y
    else (* max -0 0 is 0 *)
      Int64.logand x y

(* abs, neg, copysign are purely bitwise operations, even on NaN values *)
let abs x = Int64.logand x Int64.max_int

let neg x = Int64.logxor x Int64.min_int

let copy_sign x y = Int64.logor (abs x) (Int64.logand y Int64.min_int)

let eq x y =
  let x = to_float x in
  let y = to_float y in
  if Float.is_nan x || Float.is_nan y then false else Float.compare x y = 0

let ne x y =
  let x = to_float x in
  let y = to_float y in
  if Float.is_nan x || Float.is_nan y then true else Float.compare x y <> 0

let lt x y =
  let x = to_float x in
  let y = to_float y in
  if Float.is_nan x || Float.is_nan y then false else Float.compare x y < 0

let gt x y =
  let x = to_float x in
  let y = to_float y in
  if Float.is_nan x || Float.is_nan y then false else Float.compare x y > 0

let le x y =
  let x = to_float x in
  let y = to_float y in
  if Float.is_nan x || Float.is_nan y then false else Float.compare x y <= 0

let ge x y =
  let x = to_float x in
  let y = to_float y in
  if Float.is_nan x || Float.is_nan y then false else Float.compare x y >= 0

(*
 * Compare mantissa of two floats in string representation (hex or dec).
 * This is a gross hack to detect rounding during parsing of floats.
 *)
let is_hex = function '0' .. '9' | 'A' .. 'F' -> true | _ -> false

let is_exp hex c = Char.compare c (if hex then 'P' else 'E') = 0

let at_end hex s i = i = String.length s || is_exp hex s.[i]

let rec skip_non_hex s i =
  (* to skip sign, 'x', '.', '_', etc. *)
  if at_end true s i || is_hex s.[i] then i else skip_non_hex s (i + 1)

let rec skip_zeroes s i =
  let i' = skip_non_hex s i in
  if at_end true s i' || Char.compare s.[i'] '0' <> 0 then i'
  else skip_zeroes s (i' + 1)

let rec compare_mantissa_str' hex s1 i1 s2 i2 =
  let i1' = skip_non_hex s1 i1 in
  let i2' = skip_non_hex s2 i2 in
  match (at_end hex s1 i1', at_end hex s2 i2') with
  | true, true -> 0
  | true, false -> if at_end hex s2 (skip_zeroes s2 i2') then 0 else -1
  | false, true -> if at_end hex s1 (skip_zeroes s1 i1') then 0 else 1
  | false, false -> (
    match Char.compare s1.[i1'] s2.[i2'] with
    | 0 -> compare_mantissa_str' hex s1 (i1' + 1) s2 (i2' + 1)
    | n -> n )

let compare_mantissa_str hex s1 s2 =
  let s1' = String.uppercase_ascii s1 in
  let s2' = String.uppercase_ascii s2 in
  compare_mantissa_str' hex s1' (skip_zeroes s1' 0) s2' (skip_zeroes s2' 0)

(*
 * Convert a string to a float in target precision by going through
 * OCaml's 64 bit floats. This may incur double rounding errors in edge
 * cases, i.e., when rounding to target precision involves a tie that
 * was created by earlier rounding during parsing to float. If both
 * end up rounding in the same direction, we would "over round".
 * This function tries to detect this case and correct accordingly.
 *)
let float_of_string_prevent_double_rounding s =
  (* First parse to a 64 bit float. *)
  let z =
    match float_of_string_opt s with None -> assert false | Some z -> z
  in
  (* If value is already infinite we are done. *)
  if Float.equal (abs_float z) (1.0 /. 0.0) then z
  else
    (* Else, bit twiddling to see what rounding to target precision will do. *)
    let open Int64 in
    let bits = bits_of_float z in
    let lsb = shift_left 1L 0 in
    (* Check for tie, i.e. whether the bits right of target LSB are 10000... *)
    let tie = shift_right lsb 1 in
    let mask = lognot (shift_left (-1L) 0) in
    (* If we have no tie, we are good. *)
    if Int64.ne (logand bits mask) tie then z
    else
      (* Else, define epsilon to be the value of the tie bit. *)
      let exp = float_of_bits (logand bits 0xfff0_0000_0000_0000L) in
      let eps = float_of_bits (logor tie (bits_of_float exp)) -. exp in
      (* Convert 64 bit float back to string to compare to input. *)
      let hex = String.contains s 'x' in
      let s' =
        if not hex then Fmt.str "%.*g" (String.length s) z
        else
          let m =
            logor (logand bits 0xf_ffff_ffff_ffffL) 0x10_0000_0000_0000L
          in
          (* Shift mantissa to match msb position in most significant hex digit *)
          let i = skip_zeroes (String.uppercase_ascii s) 0 in
          if i = String.length s then Fmt.str "%.*g" (String.length s) z
          else
            let sh =
              match s.[i] with
              | '1' -> 0
              | '2' .. '3' -> 1
              | '4' .. '7' -> 2
              | _ -> 3
            in
            Fmt.str "%Lx" (shift_left m sh)
      in
      (* - If mantissa became larger, float was rounded up to tie already;
       *   round-to-even might round up again: sub epsilon to round down.
       * - If mantissa became smaller, float was rounded down to tie already;
       *   round-to-even migth round down again: add epsilon to round up.
       * - If tie is not the result of prior rounding, then we are good.
       *)
      match compare_mantissa_str hex s s' with
      | -1 -> z -. eps
      | 1 -> z +. eps
      | _ -> z

let of_signless_string s =
  if String.equal s "inf" then pos_inf
  else if String.equal s "nan" then pos_nan
  else if String.length s > 6 && String.equal (String.sub s 0 6) "nan:0x" then
    let x = Int64.of_string_exn (String.sub s 4 (String.length s - 4)) in
    if Int64.eq x Int64.zero then Fmt.failwith "nan payload must not be zero"
    else if Int64.ne (Int64.logand x bare_nan) Int64.zero then
      Fmt.failwith "nan payload must not overlap with exponent bits"
    else if Int64.lt x Int64.zero then
      Fmt.failwith "nan payload must not overlap with sign bit"
    else Int64.logor x bare_nan
  else
    let s' = String.concat "" (String.split_on_char '_' s) in
    let x = of_float (float_of_string_prevent_double_rounding s') in
    if is_inf x then Fmt.failwith "of_string" else x

let of_string_exn s =
  if String.equal s "" then Fmt.failwith "of_string_exn"
  else if Char.equal s.[0] '+' || Char.equal s.[0] '-' then
    let x = of_signless_string (String.sub s 1 (String.length s - 1)) in
    if Char.equal s.[0] '+' then x else neg x
  else of_signless_string s

let of_string s = try Some (of_string_exn s) with _ -> None

(* String conversion that groups digits for readability *)

let is_digit = function '0' .. '9' -> true | _ -> false

let is_hex_digit = function 'a' .. 'f' -> true | _ -> false

let rec add_digits buf s i j k n =
  if i < j then begin
    if k = 0 then Buffer.add_char buf '_';
    Buffer.add_char buf s.[i];
    add_digits buf s (i + 1) j ((k + n - 1) mod n) n
  end

let group_digits =
  let rec find_from_opt f s i =
    if i = String.length s then None
    else if f s.[i] then Some i
    else find_from_opt f s (i + 1)
  in
  fun is_digit n s ->
    let isnt_digit c = not (is_digit c) in
    let len = String.length s in
    let x = Option.value (find_from_opt (Char.equal 'x') s 0) ~default:0 in
    let mant = Option.value (find_from_opt is_digit s x) ~default:len in
    let point = Option.value (find_from_opt isnt_digit s mant) ~default:len in
    let frac = Option.value (find_from_opt is_digit s point) ~default:len in
    let exp = Option.value (find_from_opt isnt_digit s frac) ~default:len in
    let buf = Buffer.create (len * (n + 1) / n) in
    Buffer.add_substring buf s 0 mant;
    add_digits buf s mant point (((point - mant) mod n) + n) n;
    Buffer.add_substring buf s point (frac - point);
    add_digits buf s frac exp n n;
    Buffer.add_substring buf s exp (len - exp);
    Buffer.contents buf

(* TODO: convert all the following to a proper use of Format and stop concatenating strings *)
let to_string' convert is_digit n x =
  Fmt.str "%s%s"
    (if Int64.lt x Int64.zero then "-" else "")
    ( if is_nan x then
        let payload = Int64.logand (abs x) (Int64.lognot bare_nan) in
        Fmt.str "%s%s" "nan:0x"
          (group_digits is_hex_digit 4 (to_hex_string payload))
      else
        let s = convert (to_float (abs x)) in
        group_digits is_digit n
          (if Char.equal s.[String.length s - 1] '.' then Fmt.str "%s0" s else s)
    )

let to_string = to_string' (Fmt.str "%.17g") is_digit 3

let to_hex_string x =
  if is_inf x then to_string x else to_string' (Fmt.str "%h") is_hex_digit 4 x

let pp fmt v = Fmt.string fmt (to_string v)
