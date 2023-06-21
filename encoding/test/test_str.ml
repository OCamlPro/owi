open Encoding

let solver = Batch.create ()
let encode e = try ignore (Z3_mappings.encode_expr e) with exn -> raise exn
let abc = Strings.mk_val "abc"
let x = Expression.mk_symbol_s `StrType "x"
let zero = Integer.mk_val 0
let two = Integer.mk_val 2

(* Encoding *)
let%test_unit _ = encode abc
let%test_unit _ = encode x

(* Satisfiability *)
let%test "test_concrete_len" =
  Batch.check_sat solver
    [ Integer.mk_ge (Strings.mk_len x) (Strings.mk_len abc) ]

let%test "test_constrained_len" =
  not
    (Batch.check_sat solver
       [
         Integer.mk_eq (Strings.mk_len x) (Integer.mk_val 4);
         Integer.mk_eq (Strings.mk_len x) (Strings.mk_len abc);
       ])

let%test "test_concrete_substr" =
  let pc =
    [
      Strings.mk_eq
        (Strings.mk_substr abc ~pos:zero ~len:two)
        (Strings.mk_val "ab");
    ]
  in
  Batch.check_sat solver pc

let%test "test_symb_substr" =
  let pc =
    [
      Strings.mk_eq x abc;
      Integer.mk_eq
        (Strings.mk_len (Strings.mk_substr x ~pos:zero ~len:two))
        (Integer.mk_val 2);
    ]
  in
  Some (Value.Str "abc") = Batch.eval solver x pc
