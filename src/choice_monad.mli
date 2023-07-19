module type T = Choice_monad_intf.Complete
    with type thread := Thread.t
     and module V := Sym_value.S

module List : T

module Seq : T

module Explicit : sig
  include T
  val run_up_to : depth : int -> 'a t -> Thread.t -> ('a * Thread.t) Stdlib.Seq.t
end

val choices : (module T) list
