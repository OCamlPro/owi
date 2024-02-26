(* Taken and modified from https://github.com/WebAssembly/spec/tree/main/interpreter *)

include Stdlib.Int64

let clz =
  Some
    (fun n -> Stdlib.Int64.of_int (Ocaml_intrinsics.Int64.count_leading_zeros n))

let ctz n = Stdlib.Int64.of_int (Ocaml_intrinsics.Int64.count_trailing_zeros n)

(* Taken from Base: https://github.com/janestreet/base *)
let popcnt =
  let ( + ) = add in
  let ( - ) = sub in
  let ( * ) = mul in
  let ( lsr ) = shift_right_logical in
  let ( land ) = logand in
  let m1 = 0x5555555555555555L in
  (* 0b01010101... *)
  let m2 = 0x3333333333333333L in
  (* 0b00110011... *)
  let m4 = 0x0f0f0f0f0f0f0f0fL in
  (* 0b00001111... *)
  let h01 = 0x0101010101010101L in
  (* 1 bit set per byte *)
  fun [@inline] x ->
    (* gather the bit count for every pair of bits *)
    let x = x - ((x lsr 1) land m1) in
    (* gather the bit count for every 4 bits *)
    let x = (x land m2) + ((x lsr 2) land m2) in
    (* gather the bit count for every byte *)
    let x = (x + (x lsr 4)) land m4 in
    (* sum the bit counts in the top byte and shift it down *)
    (x * h01) lsr 56

(*
   * Unsigned comparison in terms of signed comparison.
   *)
let cmp_u x op y = op (add x min_int) (add y min_int)

(*
 * Unsigned division and remainder in terms of signed division; algorithm from
 * Hacker's Delight, Second Edition, by Henry S. Warren, Jr., section 9-3
 * "Unsigned Short Division from Signed Division".
 *)
let divrem_u n d =
  if d = zero then raise Division_by_zero
  else
    let t = shift_right d 63 in
    let n' = logand n (lognot t) in
    let q = shift_left (div (shift_right_logical n' 1) d) 1 in
    let r = sub n (mul q d) in
    if cmp_u r ( < ) d then (q, r) else (add q one, sub r d)

(* We don't override min_int and max_int since those are used
 * by other functions (like parsing), and rely on it being
 * min/max for int32 *)
(* The smallest signed |bitwidth|-bits int. *)
let low_int = shift_left minus_one 63

(* The largest signed |bitwidth|-bits int. *)
let high_int = logxor low_int minus_one

(* WebAssembly's shifts mask the shift count according to the bitwidth. *)
let shift f x y = f x (to_int (logand y (of_int 63)))

let shl x y = shift shift_left x y

let shr_s x y = shift shift_right x y

let shr_u x y = shift shift_right_logical x y

let rotl x y =
  let n = logand y 63L in
  logor (shl x n) (shr_u x (sub 64L n))

let rotr x y =
  let n = logand y 63L in
  logor (shr_u x n) (shl x (sub 64L n))

let extend_s n x =
  let shift = 64 - n in
  shift_right (shift_left x shift) shift

let eq (x : int64) y = x = y

let ne (x : int64) y = x <> y

let lt (x : int64) y = x < y

let gt (x : int64) y = x > y

let le (x : int64) y = x <= y

let ge (x : int64) y = x >= y

let lt_u x y = cmp_u x ( < ) y

let le_u x y = cmp_u x ( <= ) y

let gt_u x y = cmp_u x ( > ) y

let ge_u x y = cmp_u x ( >= ) y

(* String conversion that allows leading signs and unsigned values *)

let require b = if not b then Log.err "of_string (int64)"

let dec_digit = function
  | '0' .. '9' as c -> Char.code c - Char.code '0'
  | _ -> Log.err "of_string"

let hex_digit = function
  | '0' .. '9' as c -> Char.code c - Char.code '0'
  | 'a' .. 'f' as c -> 0xa + Char.code c - Char.code 'a'
  | 'A' .. 'F' as c -> 0xa + Char.code c - Char.code 'A'
  | _ -> Log.err "of_string"

let max_upper, max_lower = divrem_u minus_one 10L

let of_string s =
  let len = String.length s in
  let rec parse_hex i num =
    if i = len then num
    else if s.[i] = '_' then parse_hex (i + 1) num
    else
      let digit = of_int (hex_digit s.[i]) in
      require (le_u num (shr_u minus_one (of_int 4)));
      parse_hex (i + 1) (logor (shift_left num 4) digit)
  in
  let rec parse_dec i num =
    if i = len then num
    else if s.[i] = '_' then parse_dec (i + 1) num
    else
      let digit = of_int (dec_digit s.[i]) in
      require (lt_u num max_upper || (num = max_upper && le_u digit max_lower));
      parse_dec (i + 1) (add (mul num 10L) digit)
  in
  let parse_int i =
    require (len - i > 0);
    if i + 2 <= len && s.[i] = '0' && s.[i + 1] = 'x' then
      parse_hex (i + 2) zero
    else parse_dec i zero
  in
  require (len > 0);
  let parsed =
    match s.[0] with
    | '+' -> parse_int 1
    | '-' ->
      let n = parse_int 1 in
      require (sub n one >= minus_one);
      neg n
    | _ -> parse_int 0
  in
  require (low_int <= parsed && parsed <= high_int);
  parsed

let eq_const (i : int64) j = i = j
