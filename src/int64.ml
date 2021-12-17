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

(*
 * Unsigned comparison in terms of signed comparison.
 *)
let cmp_u x op y = op (add x min_int) (add y min_int)

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
