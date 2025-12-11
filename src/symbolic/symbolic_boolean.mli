include Boolean_intf.T

val of_expr : Smtml.Expr.t -> t

val to_expr : t -> Smtml.Expr.t

val ite : t -> if_true:Smtml.Expr.t -> if_false:Smtml.Expr.t -> Smtml.Expr.t
