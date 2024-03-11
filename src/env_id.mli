(** A module defining a type of small identifier and a way to obtain a fresh
    identifier *)

(** An identifier *)
type t

(** A collection of identifiers *)
type 'a collection

(** The empty collection *)
val empty : 'a collection

(** Add the successful result of a fallible computation to the collection *)
val with_fresh_id :
  'a collection -> (t -> ('a * 'b) Result.t) -> ('a collection * 'b) Result.t

(** Get the 'a value identified by t from the collection *)
val get : t -> 'a collection -> 'a

(** A specialized Map where keys are identifiers *)
module Map : Map.S with type key = t

(** A specialized Hashtbl where keys are identifiers *)
module Tbl : Hashtbl.S with type key = t
