exception Assertion of Encoding.Expr.t * Thread.t

module type T =
  Choice_monad_intf.Complete
    with type thread := Thread.t
     and module V := Symbolic_value.S

module type T_trap =
  Choice_monad_intf.Complete_with_trap
    with type thread := Thread.t
     and module V := Symbolic_value.S

module CList : T

module CSeq : T

module Explicit : sig
  include T_trap

  val run_up_to : depth:int -> 'a t -> Thread.t -> ('a * Thread.t) Seq.t
end

module MT : T_trap

val choices : (module T) list
