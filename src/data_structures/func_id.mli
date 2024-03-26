open Types

type t

type 'a collection

val empty : 'a collection

val add : 'a -> simplified func_type -> 'a collection -> t * 'a collection

val get : t -> 'a collection -> 'a

val get_typ : t -> 'a collection -> simplified func_type
