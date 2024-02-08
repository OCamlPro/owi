exception Assertion of Encoding.Expr.t * Thread.t

module Minimalist : sig
  include
    Choice_intf.Complete_without_run
      with type thread := Thread.t
       and module V := Symbolic_value.S

  type err = private
    | Assert_fail
    | Trap of Trap.t

  val run_minimalist : 'a t -> Thread.t -> ('a, err) Stdlib.Result.t * Thread.t
end

module MT :
  Choice_intf.Complete_with_trap
    with type thread := Thread.t
     and module V := Symbolic_value.S
