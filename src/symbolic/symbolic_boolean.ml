type t = Smtml.Expr.t

let false_ = Smtml.Expr.Bool.false_

let true_ = Smtml.Expr.Bool.true_

let of_concrete (i : bool) : t = if i then true_ else false_ [@@inline]

let not e = Smtml.Expr.Bool.not e [@@inline]

let or_ e1 e2 = Smtml.Expr.Bool.or_ e1 e2 [@@inline]

let and_ e1 e2 = Smtml.Expr.Bool.and_ e1 e2 [@@inline]

let pp = Smtml.Expr.pp

let ite c ~if_true ~if_false = Smtml.Expr.Bool.ite c if_true if_false [@@inline]

let of_expr v = v [@@inline]

let to_expr v = v [@@inline]
