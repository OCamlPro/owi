
module I = Interpret_functor.Make(Value_test.P) [@@inlined hint]

module S = Interpret_functor.Make(Sym_state.P) [@@inlined hint]
