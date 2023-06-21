open Expression
open Types

exception Error of string

val mk_val : float -> num_type -> expr
(** [mk_val f] creates a concrete floating-point value. *)

val mk_neg : expr -> num_type -> expr
(** [mk_neg f] create an expression representing [-f]. *)

val mk_abs : expr -> num_type -> expr
(** [mk_abs f] create an expression representing [abs(f)]. *)

val mk_sqrt : expr -> num_type -> expr
(** [mk_sqrt f] create an expression representing [sqrt(f)]. *)

val mk_nearest : expr -> num_type -> expr
(** [mk_nearest f] create an expression representing [round_nearest(f)]. *)

val mk_is_nan : expr -> num_type -> expr
(** [mk_is_nan f] create an expression representing [is_nan(f)]. *)

val mk_add : expr -> expr -> num_type -> expr
(** [mk_add f1 f2] create an expression representing [f1 + f2]. *)

val mk_sub : expr -> expr -> num_type -> expr
(** [mk_sub f1 f2] create an expression representing [f1 - f2]. *)

val mk_mul : expr -> expr -> num_type -> expr
(** [mk_mul f1 f2] create an expression representing [f1 * f2]. *)

val mk_div : expr -> expr -> num_type -> expr
(** [mk_div f1 f2] create an expression representing [f1 / f2]. *)

val mk_min : expr -> expr -> num_type -> expr
(** [mk_min f1 f2] create an expression representing [min f1 f2]. *)

val mk_max : expr -> expr -> num_type -> expr
(** [mk_max f1 f2] create an expression representing [max f1 f2]. *)

val mk_rem : expr -> expr -> num_type -> expr
(** [mk_rem f1 f2] create an expression representing [f1 % f2]. *)

val mk_eq : expr -> expr -> num_type -> expr
(** [mk_eq f1 f2] create an expression representing [f1 = f2]. *)

val mk_ne : expr -> expr -> num_type -> expr
(** [mk_ne f1 f2] create an expression representing [not (f1 = f2)]. *)

val mk_lt : expr -> expr -> num_type -> expr
(** [mk_lt f1 f2] create an expression representing [f1 < f2]. *)

val mk_le : expr -> expr -> num_type -> expr
(** [mk_le f1 f2] create an expression representing [f1 <= f2]. *)

val mk_gt : expr -> expr -> num_type -> expr
(** [mk_gt f1 f2] create an expression representing [f1 > f2]. *)

val mk_ge : expr -> expr -> num_type -> expr
(** [mk_ge f1 f2] create an expression representing [f1 >= f2]. *)
