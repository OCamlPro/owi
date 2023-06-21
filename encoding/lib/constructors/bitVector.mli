open Expression
open Types

exception Error of string

val mk_val : int -> num_type -> expr
(** [mk_val i] creates a concrete bitvector value. *)

val mk_not : expr -> num_type -> expr
(** [mk_not i] create an expression representing [not i]. *)

val mk_clz : expr -> num_type -> expr
(** [mk_clz i] create an expression representing [clz i]. *)

val mk_add : expr -> expr -> num_type -> expr
(** [mk_add i1 i2] create an expression representing [i1 + i2]. *)

val mk_sub : expr -> expr -> num_type -> expr
(** [mk_sub i1 i2] create an expression representing [i1 - i2]. *)

val mk_mul : expr -> expr -> num_type -> expr
(** [mk_mul i1 i2] create an expression representing [i1 * i2]. *)

val mk_div_u : expr -> expr -> num_type -> expr
(** [mk_div_u i1 i2] create an expression representing unsigned [i1 / i2]. *)

val mk_div_s : expr -> expr -> num_type -> expr
(** [mk_div_s i1 i2] create an expression representing signed [i1 / i2]. *)

val mk_rem_u : expr -> expr -> num_type -> expr
(** [mk_rem_u i1 i2] create an expression representing unsigned [i1 % i2]. *)

val mk_rem_s : expr -> expr -> num_type -> expr
(** [mk_rem_s i1 i2] create an expression representing signed [i1 % i2]. *)

val mk_shl : expr -> expr -> num_type -> expr
(** [mk_shl i1 i2] create an expression representing [i1 << i2]. *)

val mk_shr_u : expr -> expr -> num_type -> expr
(** [mk_shr_u i1 i2] create an expression representing signed [i1 >> i2]. *)

val mk_shr_s : expr -> expr -> num_type -> expr
(** [mk_shr_u i1 i2] create an expression representing unsigned [i1 >> i2]. *)

val mk_and : expr -> expr -> num_type -> expr
(** [mk_and i1 i2] create an expression representing unsigned [i1 & i2]. *)

val mk_or : expr -> expr -> num_type -> expr
(** [mk_or i1 i2] create an expression representing unsigned [i1 | i2]. *)

val mk_xor : expr -> expr -> num_type -> expr
(** [mk_xor i1 i2] create an expression representing unsigned [i1 ^ i2]. *)

val mk_eq : expr -> expr -> num_type -> expr
(** [mk_eq i1 i2] create an expression representing [i1 = i2]. *)

val mk_ne : expr -> expr -> num_type -> expr
(** [mk_ne i1 i2] create an expression representing [not (i1 = i2)]. *)

val mk_lt_u : expr -> expr -> num_type -> expr
(** [mk_lt_u i1 i2] create an expression representing unsigned [i1 < i2]. *)

val mk_lt_s : expr -> expr -> num_type -> expr
(** [mk_lt_s i1 i2] create an expression representing signed [i1 < i2]. *)

val mk_le_u : expr -> expr -> num_type -> expr
(** [mk_le_u i1 i2] create an expression representing unsigned [i1 <= i2]. *)

val mk_le_s : expr -> expr -> num_type -> expr
(** [mk_le_s i1 i2] create an expression representing signed [i1 <= i2]. *)

val mk_gt_u : expr -> expr -> num_type -> expr
(** [mk_gt_u i1 i2] create an expression representing unsigned [i1 > i2]. *)

val mk_gt_s : expr -> expr -> num_type -> expr
(** [mk_gt_s i1 i2] create an expression representing signed [i1 > i2]. *)

val mk_ge_u : expr -> expr -> num_type -> expr
(** [mk_ge_u i1 i2] create an expression representing unsigned [i1 >= i2]. *)

val mk_ge_s : expr -> expr -> num_type -> expr
(** [mk_ge_s i1 i2] create an expression representing signed [i1 >= i2]. *)
