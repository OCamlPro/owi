exception Assertion of Encoding.Expr.t * Thread.t

module MT :
  Choice_intf.Complete_with_trap
    with type thread := Thread.t
     and module V := Symbolic_value.S
