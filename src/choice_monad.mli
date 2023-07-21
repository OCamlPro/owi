exception Assertion of Choice_monad_intf.assertion * Thread.t

module type T = Choice_monad_intf.Complete
    with type thread := Thread.t
     and module V := Sym_value.S

module type T_trap = Choice_monad_intf.Complete_with_trap
    with type thread := Thread.t
     and module V := Sym_value.S

module List : T

module Seq : T

module Explicit : sig
  include T_trap
  val run_up_to : depth : int -> 'a t -> Thread.t -> ('a * Thread.t) Stdlib.Seq.t
end

val choices : (module T) list
