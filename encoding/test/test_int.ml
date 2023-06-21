open Encoding

let solver = Batch.create ()
let encode e = try ignore (Z3_mappings.encode_expr e) with exn -> raise exn
let one = Integer.mk_val Int.one
let minus_one = Integer.mk_val Int.minus_one
let zero = Integer.mk_val Int.zero
let x = Expression.mk_symbol_s `IntType "x"

(* Encoding *)
let%test_unit _ = encode one
let%test_unit _ = encode minus_one
let%test_unit _ = encode zero
let%test_unit _ = encode x

(* Satisfiability *)
let%test _ = Batch.check_sat solver [ Integer.mk_gt x zero ]
let%test _ = Batch.check_sat solver [ Integer.mk_gt one minus_one ]
let%test _ = Batch.check_sat solver [ Integer.mk_eq (Integer.mk_pow x one) x ]
