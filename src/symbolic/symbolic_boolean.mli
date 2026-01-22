include Boolean_intf.T with type t = bool Smtml.Typed.t

val ite :
  t -> if_true:'a Smtml.Typed.t -> if_false:'a Smtml.Typed.t -> 'a Smtml.Typed.t
