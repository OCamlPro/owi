module List :
  Choice_monad_intf.Complete
    with type thread := Thread.t
     and module V := Sym_value.S
