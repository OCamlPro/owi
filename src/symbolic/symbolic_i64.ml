include Smtml.Typed.Bitv64

let eq_concrete (e : t) (c : Int64.t) : Symbolic_boolean.t =
  let c = of_int64 c in
  Smtml.Typed.Bitv64.eq c e
