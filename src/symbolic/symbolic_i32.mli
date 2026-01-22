include
  I32_intf.T
    with type t = Smtml.Typed.bitv32 Smtml.Typed.t
     and type boolean := bool Smtml.Typed.t
     and type i64 := Smtml.Typed.bitv64 Smtml.Typed.t
     and type f32 := Smtml.Typed.float32 Smtml.Typed.t
     and type f64 := Smtml.Typed.float64 Smtml.Typed.t

val symbol : Smtml.Symbol.t -> t
