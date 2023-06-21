open Encoding

let opt = Optimizer.create ()
let x = Expression.mk_symbol_s `IntType "x"

(* Satisfiability *)
let%test "opt_min" =
  let pc =
    [ Integer.mk_ge x (Integer.mk_val 0); Integer.mk_lt x (Integer.mk_val 5) ]
  in
  Some (Value.Int 0) = Optimizer.minimize opt x pc

let%test "opt_max" =
  let pc =
    [ Integer.mk_ge x (Integer.mk_val 0); Integer.mk_lt x (Integer.mk_val 5) ]
  in
  Some (Value.Int 4) = Optimizer.maximize opt x pc
