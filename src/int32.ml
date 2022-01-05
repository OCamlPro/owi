include Stdlib.Int32

let clz = Ocaml_intrinsics.Int32.count_leading_zeros

let ctz = Ocaml_intrinsics.Int32.count_trailing_zeros

(* Taken from Base *)
let popcnt =
  let mask = 0xffff_ffffL in
  fun [@inline] x -> Int64.popcnt (Int64.logand (Int64.of_int32 x) mask)

let to_hex_string = Printf.sprintf "%lx"

let of_int64 = Int64.to_int32

let to_int64 = Int64.of_int32

exception Overflow

(* Unsigned comparison in terms of signed comparison. *)
let cmp_u x op y = op (add x min_int) (add y min_int)

(* Unsigned division and remainder in terms of signed division; algorithm from
 * Hacker's Delight, Second Edition, by Henry S. Warren, Jr., section 9-3
 * "Unsigned Short Division from Signed Division". *)
let divrem_u n d =
  if d = zero then raise Division_by_zero
  else
    let t = shift_right d (32 - 1) in
    let n' = logand n (lognot t) in
    let q = shift_left (div (shift_right_logical n' 1) d) 1 in
    let r = sub n (mul q d) in
    if cmp_u r ( < ) d then (q, r) else (add q one, sub r d)

(* If bit (32 - 1) is set, sx will sign-extend t to maintain the
 * invariant that small ints are stored sign-extended inside a wider int. *)
let sx x =
  Int64.to_int32 @@ Int64.(shift_right (shift_left (Int64.of_int32 x) 32) 32)

(* We don't override min_int and max_int since those are used
 * by other functions (like parsing), and rely on it being
 * min/max for int32 *)
(* The smallest signed |32|-bits int. *)
let low_int = shift_left minus_one 31

(* The largest signed |32|-bits int. *)
let high_int = logxor low_int minus_one

(* result is floored (which is the same as truncating for unsigned values) *)
let div_u x y =
  let q, _r = divrem_u x y in
  q

let rem_u x y =
  let _q, r = divrem_u x y in
  r

(* WebAssembly's shifts mask the shift count according to the 32. *)
let shift f x y = f x (to_int (logand y 31l))

let shl x y = sx (shift shift_left x y)

let shr_s x y = shift shift_right x y

let shr_u x y = sx (shift shift_right_logical x y)

(* We must mask the count to implement rotates via shifts. *)
let clamp_rotate_count n = to_int (logand n 31l)

let rotl x y =
  let n = clamp_rotate_count y in
  logor (shl x (of_int n)) (shr_u x (of_int (32 - n)))

let rotr x y =
  let n = clamp_rotate_count y in
  logor (shr_u x (of_int n)) (shl x (of_int (32 - n)))

let extend_s n x =
  let shift = 32 - n in
  shift_right (shift_left x shift) shift

let lt_u x y = cmp_u x ( < ) y

let le_u x y = cmp_u x ( <= ) y

let gt_u x y = cmp_u x ( > ) y

let ge_u x y = cmp_u x ( >= ) y

let saturate_s x = sx (min (max x low_int) high_int)

let saturate_u x = sx (min (max x zero) minus_one)

(* String conversion that allows leading signs and unsigned values *)

let require b = if not b then failwith "of_string"

let dec_digit = function
  | '0' .. '9' as c -> Char.code c - Char.code '0'
  | _ -> failwith "of_string"

let hex_digit = function
  | '0' .. '9' as c -> Char.code c - Char.code '0'
  | 'a' .. 'f' as c -> 0xa + Char.code c - Char.code 'a'
  | 'A' .. 'F' as c -> 0xa + Char.code c - Char.code 'A'
  | _ -> failwith "of_string"

let max_upper, max_lower = divrem_u minus_one 10l

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
      parse_dec (i + 1) (add (mul num 10l) digit)
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
