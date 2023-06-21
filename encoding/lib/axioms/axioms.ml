let axioms =
  let x = Symbol.mk_symbol `StrType "x" in
  [
    Expression.Quantifier
      ( Expression.Forall,
        [ x ],
        Strings.mk_eq
          (Integer.mk_to_string (Integer.mk_of_string (Expression.mk_symbol x)))
          (Expression.mk_symbol x),
        [ [ Integer.mk_of_string (Expression.mk_symbol x) ] ] );
  ]
