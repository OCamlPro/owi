open Expression

val mk_val : int -> expr
(** [mk_val i] creates a concrete integer value. *)

val mk_neg : expr -> expr
(** [mk_neg i] create an expression representing [-i]. *)

val mk_add : expr -> expr -> expr
(** [mk_add i1 i2] create an expression representing [i1 + i2]. *)

val mk_sub : expr -> expr -> expr
(** [mk_sub i1 i2] create an expression representing [i1 - i2]. *)

val mk_mul : expr -> expr -> expr
(** [mk_mul i1 i2] create an expression representing [i1 * i2]. *)

val mk_div : expr -> expr -> expr
(** [mk_div i1 i2] create an expression representing [i1 / i2]. *)

val mk_rem : expr -> expr -> expr
(** [mk_rem i1 i2] create an expression representing [i1 % i2]. *)

val mk_shl : expr -> expr -> expr
(** [mk_shl i1 i2] create an expression representing [i1 << i2]. *)

val mk_shr_a : expr -> expr -> expr
(** [mk_shr_a i1 i2] create an expression representing [i1 >> i2]. *)

val mk_shr_l : expr -> expr -> expr
(** [mk_shr_l i1 i2] create an expression representing [i1 >> i2]. *)

val mk_and : expr -> expr -> expr
(** [mk_and i1 i2] create an expression representing [i1 & i2]. *)

val mk_or : expr -> expr -> expr
(** [mk_or i1 i2] create an expression representing [i1 | i2]. *)

val mk_xor : expr -> expr -> expr
(** [mk_xor i1 i2] create an expression representing [i1 xor i2]. *)

val mk_pow : expr -> expr -> expr
(** [mk_pow i1 i2] create an expression representing [i1 ** i2]. *)

val mk_eq : expr -> expr -> expr
(** [mk_eq i1 i2] create an expression representing [i1 = i2]. *)

val mk_ne : expr -> expr -> expr
(** [mk_ne i1 i2] create an expression representing [not (i1 = i2)]. *)

val mk_lt : expr -> expr -> expr
(** [mk_lt i1 i2] create an expression representing [i1 < i2]. *)

val mk_le : expr -> expr -> expr
(** [mk_le i1 i2] create an expression representing [i1 <= i2]. *)

val mk_gt : expr -> expr -> expr
(** [mk_gt i1 i2] create an expression representing [i1 > i2]. *)

val mk_ge : expr -> expr -> expr
(** [mk_ge i1 i2] create an expression representing [i1 >= i2]. *)

val mk_to_string : expr -> expr
(** [mk_to_string s] create an expression representing a string *)

val mk_of_string : expr -> expr
(** [mk_of_string s] create an expression representing an integer *)

val mk_of_real : expr -> expr
(** [mk_of_real f] create an expression representing an integer *)
