include Stdlib.Int64

let clz = Ocaml_intrinsics.Int64.count_leading_zeros

let ctz = Ocaml_intrinsics.Int64.count_trailing_zeros

exception InvalidConversion

exception Overflow

(* Taken from Base *)
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
    to_int ((x * h01) lsr 56)

let bitwidth = 64

let to_hex_string = Printf.sprintf "%Lx"

let of_int64 i = i

let to_int64 i = i

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
    let t = shift_right d (bitwidth - 1) in
    let n' = logand n (lognot t) in
    let q = shift_left (div (shift_right_logical n' 1) d) 1 in
    let r = sub n (mul q d) in
    if cmp_u r ( < ) d then (q, r) else (add q one, sub r d)

type bits = t

let of_bits x = x

let to_bits x = x

let zero = zero

let one = one

let ten = of_int 10

let lognot = lognot

let abs = abs

let neg = neg

(* If bit (bitwidth - 1) is set, sx will sign-extend t to maintain the
 * invariant that small ints are stored sign-extended inside a wider int. *)
let sx x =
  let i = 64 - bitwidth in
  of_int64 (shift_right (shift_left (to_int64 x) i) i)

(* add, sub, and mul are sign-agnostic and do not trap on overflow. *)
let add x y = sx (add x y)

let sub x y = sx (sub x y)

let mul x y = sx (mul x y)

(* We don't override min_int and max_int since those are used
 * by other functions (like parsing), and rely on it being
 * min/max for int32 *)
(* The smallest signed |bitwidth|-bits int. *)
let low_int = shift_left minus_one (bitwidth - 1)

(* The largest signed |bitwidth|-bits int. *)
let high_int = logxor low_int minus_one

(* result is truncated toward zero *)
let div_s x y =
  if y = zero then raise Division_by_zero
  else if x = low_int && y = minus_one then raise Overflow
  else div x y

(* result is floored (which is the same as truncating for unsigned values) *)
let div_u x y =
  let q, _r = divrem_u x y in
  q

(* result has the sign of the dividend *)
let rem_s x y = if y = zero then raise Division_by_zero else rem x y

let rem_u x y =
  let _q, r = divrem_u x y in
  r

let avgr_u x y =
  (* Mask with bottom #bitwidth bits set *)
  let mask = shift_right_logical minus_one (64 - bitwidth) in
  let x64 = logand mask (to_int64 x) in
  let y64 = logand mask (to_int64 y) in
  of_int64 (div (add (add x64 y64) one) (of_int 2))

let and_ = logand

let or_ = logor

let xor = logxor

(* WebAssembly's shifts mask the shift count according to the bitwidth. *)
let shift f x y = f x (to_int (logand y (of_int (bitwidth - 1))))

let shl x y = sx (shift shift_left x y)

let shr_s x y = shift shift_right x y

(* Check if we are storing smaller ints. *)
let needs_extend = shl one (of_int (bitwidth - 1)) <> min_int

(*
 * When Int is used to store a smaller int, it is stored in signed extended
 * form. Some instructions require the unsigned form, which requires masking
 * away the top 32-bitwidth bits.
 *)
let as_unsigned x =
  if not needs_extend then x
  else
    (* Mask with bottom #bitwidth bits set *)
    let mask = shift_right_logical minus_one (32 - bitwidth) in
    logand x mask

let shr_u x y = sx (shift shift_right_logical (as_unsigned x) y)

(* We must mask the count to implement rotates via shifts. *)
let clamp_rotate_count n = to_int (logand n (of_int (bitwidth - 1)))

let rotl x y =
  let n = clamp_rotate_count y in
  or_ (shl x (of_int n)) (shr_u x (of_int (bitwidth - n)))

let rotr x y =
  let n = clamp_rotate_count y in
  or_ (shr_u x (of_int n)) (shl x (of_int (bitwidth - n)))

let extend_s n x =
  let shift = bitwidth - n in
  shift_right (shift_left x shift) shift

let eqz x = x = zero

let eq x y = x = y

let ne x y = x <> y

let lt_s x y = x < y

let lt_u x y = cmp_u x ( < ) y

let le_s x y = x <= y

let le_u x y = cmp_u x ( <= ) y

let gt_s x y = x > y

let gt_u x y = cmp_u x ( > ) y

let ge_s x y = x >= y

let ge_u x y = cmp_u x ( >= ) y

let saturate_s x = sx (min (max x low_int) high_int)

let saturate_u x = sx (min (max x zero) (as_unsigned minus_one))

(* add/sub for int, used for higher-precision arithmetic for I8 and I16 *)
let add_int x y =
  assert (bitwidth < 32);
  of_int (to_int x + to_int y)

let sub_int x y =
  assert (bitwidth < 32);
  of_int (to_int x - to_int y)

let add_sat_s x y = saturate_s (add_int x y)

let add_sat_u x y = saturate_u (add_int (as_unsigned x) (as_unsigned y))

let sub_sat_s x y = saturate_s (sub_int x y)

let sub_sat_u x y = saturate_u (sub_int (as_unsigned x) (as_unsigned y))

let q15mulr_sat_s x y =
  (* mul x64 y64 can overflow int64 when both are int32 min, but this is only
   * used by i16x8, so we are fine for now. *)
  assert (bitwidth < 32);
  let x64 = to_int64 x in
  let y64 = to_int64 y in
  saturate_s (of_int64 (shift_right (add (mul x64 y64) 0x4000L) 15))

let to_int_s = to_int

let of_int_s = of_int

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

let max_upper, max_lower = divrem_u minus_one ten

let sign_extend i =
  (* This module is used with I32 and I64, but the bitwidth can be less
   * than that, e.g. for I16. When used for smaller integers, the stored value
   * needs to be signed extended, e.g. parsing -1 into a I16 (backed by Int32)
   * should have all high bits set. We can do that by logor with a mask,
   * where the mask is minus_one left shifted by bitwidth. But if bitwidth
   * matches the number of bits of Rep, the shift will be incorrect.
   *   -1 (Int32) << 32 = -1
   * Then the logor will be also wrong. So we check and bail out early.
   * *)
  if not needs_extend then i
  else
    let sign_bit = logand (of_int (1 lsl (bitwidth - 1))) i in
    if sign_bit = zero then i
    else
      (* Build a sign-extension mask *)
      let sign_mask = shift_left minus_one bitwidth in
      logor sign_mask i

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
      parse_dec (i + 1) (add (mul num ten) digit)
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
      require (ge_s (sub n one) minus_one);
      neg n
    | _ -> parse_int 0
  in
  let n = sign_extend parsed in
  require (low_int <= n && n <= high_int);
  n

let of_string_s s =
  let n = of_string s in
  require (s.[0] = '-' || ge_s n zero);
  n

let of_string_u s =
  let n = of_string s in
  require (s.[0] <> '+' && s.[0] <> '-');
  n

(* String conversion that groups digits for readability *)

let rec add_digits buf s i j k n =
  if i < j then begin
    if k = 0 then Buffer.add_char buf '_';
    Buffer.add_char buf s.[i];
    add_digits buf s (i + 1) j ((k + n - 1) mod n) n
  end

let group_digits n s =
  let len = String.length s in
  let num = if s.[0] = '-' then 1 else 0 in
  let buf = Buffer.create (len * (n + 1) / n) in
  Buffer.add_substring buf s 0 num;
  add_digits buf s num len (((len - num) mod n) + n) n;
  Buffer.contents buf

let to_string_s i = group_digits 3 (to_string i)

let to_string_u i =
  if i >= zero then group_digits 3 (to_string i)
  else group_digits 3 (to_string (div_u i ten) ^ to_string (rem_u i ten))

let to_hex_string i = "0x" ^ group_digits 4 (to_hex_string i)
