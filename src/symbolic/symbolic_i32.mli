include
  I32_intf.T
    with type t = Smtml.Typed.Bitv32.t
     and type boolean := Smtml.Typed.Bool.t
     and type i64 := Smtml.Typed.Bitv64.t
     and type f32 := Smtml.Typed.Float32.t
     and type f64 := Smtml.Typed.Float64.t

val symbol : Smtml.Symbol.t -> t
