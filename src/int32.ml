include Stdlib.Int32

let clz = Ocaml_intrinsics.Int32.count_leading_zeros

let ctz = Ocaml_intrinsics.Int32.count_trailing_zeros

(* Taken from Base *)
let popcnt =
  let mask = 0xffff_ffffL in
  fun [@inline] x -> Int64.popcnt (Int64.logand (Int64.of_int32 x) mask)
