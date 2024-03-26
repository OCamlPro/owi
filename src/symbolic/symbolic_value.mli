type externref

type ref_value =
  | Funcref of Func_intf.t option
  | Externref of externref option

include
  Value_intf.T
    with type ref_value := ref_value
    with type vbool = Encoding.Expr.t
     and type int32 = Encoding.Expr.t
     and type int64 = Encoding.Expr.t
     and type float32 = Encoding.Expr.t
     and type float64 = Encoding.Expr.t

module Bool : sig
  include module type of Bool

  val select_expr :
       Encoding.Expr.t
    -> if_true:Encoding.Expr.t
    -> if_false:Encoding.Expr.t
    -> Encoding.Expr.t
end
