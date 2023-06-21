open Expression

val mk_val : float -> expr
(** [mk_val f] creates a concrete floating-point value. *)

val mk_neg : expr -> expr
(** [mk_neg f] create an expression representing [-f]. *)

val mk_abs : expr -> expr
(** [mk_abs f] create an expression representing [abs(f)]. *)

val mk_sqrt : expr -> expr
(** [mk_sqrt f] create an expression representing [sqrt(f)]. *)

val mk_add : expr -> expr -> expr
(** [mk_add f1 f2] create an expression representing [f1 + f2]. *)

val mk_sub : expr -> expr -> expr
(** [mk_sub f1 f2] create an expression representing [f1 - f2]. *)

val mk_mul : expr -> expr -> expr
(** [mk_mul f1 f2] create an expression representing [f1 * f2]. *)

val mk_div : expr -> expr -> expr
(** [mk_div f1 f2] create an expression representing [f1 / f2]. *)

val mk_min : expr -> expr -> expr
(** [mk_min f1 f2] create an expression representing [min f1 f2]. *)

val mk_max : expr -> expr -> expr
(** [mk_max f1 f2] create an expression representing [max f1 f2]. *)

val mk_eq : expr -> expr -> expr
(** [mk_eq f1 f2] create an expression representing [f1 = f2]. *)

val mk_ne : expr -> expr -> expr
(** [mk_ne f1 f2] create an expression representing [not (f1 = f2)]. *)

val mk_lt : expr -> expr -> expr
(** [mk_lt f1 f2] create an expression representing [f1 < f2]. *)

val mk_le : expr -> expr -> expr
(** [mk_le f1 f2] create an expression representing [f1 <= f2]. *)

val mk_gt : expr -> expr -> expr
(** [mk_gt f1 f2] create an expression representing [f1 > f2]. *)

val mk_ge : expr -> expr -> expr
(** [mk_ge f1 f2] create an expression representing [f1 >= f2]. *)

val mk_to_string : expr -> expr
(** [mk_to_string f] create an expression representing a string *)

val mk_of_string : expr -> expr
(** [mk_of_string f] create an expression representing a real *)

val mk_of_integer : expr -> expr
(** [mk_of_integer i] create an expression representing a real *)

val mk_to_uint32 : expr -> expr
