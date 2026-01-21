include Boolean_intf.T with type t = bool Smtml.Typed.t

val of_expr : Smtml.Expr.t -> t

val to_expr : t -> Smtml.Expr.t

val ite :
  t -> if_true:'a Smtml.Typed.t -> if_false:'a Smtml.Typed.t -> 'a Smtml.Typed.t
