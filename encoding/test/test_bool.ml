open Encoding

let solver = Batch.create ()
let x = Expression.mk_symbol_s `BoolType "x"

let%test "test_not" =
  let pc = [ Boolean.mk_not (Boolean.mk_eq x (Boolean.mk_val true)) ] in
  Some (Value.Bool false) = Batch.eval solver x pc
