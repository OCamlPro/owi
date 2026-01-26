include Smtml.Typed.Bitv64

let of_concrete (i : Int64.t) : t =
  Smtml.Typed.Bitv64.v (Smtml.Bitvector.of_int64 i)

let of_int (i : int) : t = of_concrete (Int64.of_int i)

let eq_concrete (e : t) (c : Int64.t) : Symbolic_boolean.t =
  let c = of_concrete c in
  Smtml.Typed.Bitv64.eq c e
