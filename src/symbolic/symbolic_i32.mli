include
  I32_intf.T
    with type t = Smtml.Expr.t
     and type boolean := Symbolic_boolean.t
     and type i64 := Smtml.Expr.t
     and type f32 := Smtml.Expr.t
     and type f64 := Smtml.Expr.t

val symbol : Smtml.Symbol.t -> t
