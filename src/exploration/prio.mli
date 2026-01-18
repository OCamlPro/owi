type metrics

val v :
     instr_counter:int option
  -> distance_to_unreachable:int option
  -> depth:int
  -> metrics

val dummy : metrics

val low : metrics

module type T = sig
  type t

  val of_metrics : metrics -> t

  val compare : t -> t -> int

  val requires_random : Bool.t
end

module FIFO : T

module LIFO : T

module Random_prio : T

module Random_unseen_then_random : T

module Rarity : T

module Hot_path_penalty : T

module Rarity_aging : T

module Rarity_depth_aging : T

module Rarity_depth_loop_aging : T

module Rarity_depth_loop_aging_random : T

module type S = sig
  type !'a t

  (** Create a new queue *)
  val make : unit -> 'a t

  (** Add a new element to the queue *)
  val push : 'a -> metrics -> 'a t -> unit

  (** Make a new pledge, ie indicate that new elements may be pushed to the
      queue and that calls to pop should block waiting for them. *)
  val new_pledge : 'a t -> unit

  (** End one pledge. *)
  val end_pledge : 'a t -> unit

  (** Mark the queue as closed: all threads trying to pop from it will get no
      element. *)
  val close : 'a t -> unit

  (** Pop all elements from the queue in a lazy Seq.t, *)
  val read_as_seq : 'a t -> 'a Seq.t

  val work_while : ('a -> (metrics * 'a -> unit) -> unit) -> 'a t -> unit
end

module Make (_ : T) : S
