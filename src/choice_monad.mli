module type T = Choice_monad_intf.Complete
    with type thread := Thread.t
     and module V := Sym_value.S

module List : T

module Seq : T

module Explicit : T

val choices : (module T) list
