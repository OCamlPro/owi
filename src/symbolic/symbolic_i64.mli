include
  I64_intf.T
    with type t = Smtml.Expr.t
     and type boolean := bool Smtml.Typed.t
     and type i32 := Smtml.Expr.t
     and type f32 := Smtml.Typed.float32 Smtml.Typed.t
     and type f64 := Smtml.Typed.float64 Smtml.Typed.t
