open Expression

val mk_val : bool -> expr
(** [mk_val b] creates a concrete boolean value. *)

val mk_not : expr -> expr
(** [mk_not e] create an expression representing [not e]. *)

val mk_and : expr -> expr -> expr
(** [mk_and e1 e2] create an expression representing [e1 and e2]. *)

val mk_or : expr -> expr -> expr
(** [mk_or e1 e2] create an expression representing [e1 or e2]. *)

val mk_xor : expr -> expr -> expr
(** [mk_xor e1 e2] create an expression representing [e1 xor e2]. *)

val mk_eq : expr -> expr -> expr
(** [mk_eq e1 e2] create an expression representing [e1 = e2]. *)

val mk_ne : expr -> expr -> expr
(** [mk_ne e1 e2] create an expression representing [not (e1 = e2)]. *)

val mk_ite : expr -> expr -> expr -> expr
(** [mk_ite e1 e2 e3] create an expression representing 
    [if e1 then e2 else e3]*)
