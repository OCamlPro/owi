include Boolean_intf.T with type t = Smtml.Typed.Bool.t

val ite : t -> 'a Smtml.Typed.expr -> 'a Smtml.Typed.expr -> 'a Smtml.Typed.expr
