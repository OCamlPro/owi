open Expression
open Types

let mk_val (b : bool) : expr = Val (Bool b)
let mk_not (e : expr) : expr = Unop (Bool B.Not, e)
let mk_and (e1 : expr) (e2 : expr) : expr = Binop (Bool B.And, e1, e2)
let mk_or (e1 : expr) (e2 : expr) : expr = Binop (Bool B.Or, e1, e2)
let mk_xor (e1 : expr) (e2 : expr) : expr = Binop (Bool B.Xor, e1, e2)
let mk_eq (e1 : expr) (e2 : expr) : expr = Relop (Bool B.Eq, e1, e2)
let mk_ne (e1 : expr) (e2 : expr) : expr = Relop (Bool B.Ne, e1, e2)
let mk_ite (e1 : expr) (e2 : expr) (e3 : expr) = Triop (Bool B.ITE, e1, e2, e3)
