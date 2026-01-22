type t = bool Smtml.Typed.t

include Smtml.Typed.Bool

let of_concrete (i : bool) : t = if i then true_ else false_ [@@inline]

let ite c ~if_true ~if_false = Smtml.Typed.Bool.ite c if_true if_false
[@@inline]

let of_expr v = Smtml.Typed.unsafe v [@@inline]

let to_expr v = Smtml.Typed.raw v [@@inline]
