open Expression

val mk_val : String.t -> expr
(** [mk_val s] creates a concrete string value. *)

val mk_len : expr -> expr
(** [mk_len s] create an expression representing [length s]. *)

val mk_nth : expr -> expr -> expr
(** [mk_nth s i] create an expression representing [s.(i)]. *)

val mk_concat : expr -> expr -> expr
(** [mk_concat s1 s2] create an expression representing [s1 ^ s2]. *)

val mk_eq : expr -> expr -> expr
(** [mk_eq s1 s2] create an expression representing [s1 = s2]. *)

val mk_ne : expr -> expr -> expr
(** [mk_ne s1 s2] create an expression representing [not (s1 = s2)]. *)

val mk_substr : expr -> pos:expr -> len:expr -> expr
(** [mk_substr s pos len] create an expression representing the substring of 
    [s] starting in [pos] and with length [len]. *)

val mk_trim : expr -> expr
