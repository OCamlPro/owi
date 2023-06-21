open Encoding

let solver = Batch.create ()

let%test "test-to_string-eq" =
  let x = Expression.mk_symbol_s `RealType "x"
  and y = Expression.mk_symbol_s `RealType "y" in
  Batch.check_sat solver
    [ Strings.mk_eq (Real.mk_to_string x) (Real.mk_to_string y) ]
