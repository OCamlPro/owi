open Types

exception DivideByZero
exception Num of num_type
exception TypeError of int * Num.t * num_type

val eval_unop : unop -> Num.t -> Num.t
val eval_binop : binop -> Num.t -> Num.t -> Num.t
val eval_relop : relop -> Num.t -> Num.t -> bool
val eval_cvtop : cvtop -> Num.t -> Num.t
