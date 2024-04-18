type externref

type ref_value =
  | Funcref of Func_intf.t option
  | Externref of externref option

include
  Value_intf.T
    with type ref_value := ref_value
    with type vbool = Smtml.Expr.t
     and type int32 = Smtml.Expr.t
     and type int64 = Smtml.Expr.t
     and type float32 = Smtml.Expr.t
     and type float64 = Smtml.Expr.t

module Bool : sig
  include module type of Bool

  val select_expr :
       Smtml.Expr.t
    -> if_true:Smtml.Expr.t
    -> if_false:Smtml.Expr.t
    -> Smtml.Expr.t
end
