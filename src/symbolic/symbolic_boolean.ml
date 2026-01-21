type t = bool Smtml.Typed.t

let false_ = Smtml.Typed.false_

let true_ = Smtml.Typed.true_

let of_concrete (i : bool) : t = if i then true_ else false_ [@@inline]

let not e = Smtml.Typed.not_ e [@@inline]

let or_ e1 e2 = Smtml.Typed.or_ e1 e2 [@@inline]

let and_ e1 e2 = Smtml.Typed.and_ e1 e2 [@@inline]

let pp fmt e = Smtml.Expr.pp fmt (Smtml.Typed.raw e)

let ite c ~if_true ~if_false = Smtml.Typed.ite c if_true if_false [@@inline]

let of_expr v = Smtml.Typed.unsafe v [@@inline]

let to_expr v = Smtml.Typed.raw v [@@inline]
