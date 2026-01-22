type t = bool Smtml.Typed.t

let false_ = Smtml.Typed.Bool.false_

let true_ = Smtml.Typed.Bool.true_

let of_concrete (i : bool) : t = if i then true_ else false_ [@@inline]

let not e = Smtml.Typed.Bool.not e [@@inline]

let or_ e1 e2 = Smtml.Typed.Bool.or_ e1 e2 [@@inline]

let and_ e1 e2 = Smtml.Typed.Bool.and_ e1 e2 [@@inline]

let pp fmt e = Smtml.Typed.Bool.pp fmt e [@@inline]

let ite c ~if_true ~if_false = Smtml.Typed.Bool.ite c if_true if_false
[@@inline]

let of_expr v = Smtml.Typed.unsafe v [@@inline]

let to_expr v = Smtml.Typed.raw v [@@inline]
