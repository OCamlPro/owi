include Boolean_intf.T with type t = Smtml.Expr.t and type i32 := Smtml.Expr.t

val ite : t -> if_true:Smtml.Expr.t -> if_false:Smtml.Expr.t -> Smtml.Expr.t
