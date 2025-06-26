(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

module type S = sig
  type !'a t

  (** Create a new queue *)
  val make : unit -> 'a t

  (** Add a new element to the queue *)
  val push : 'a -> Prio.t -> 'a t -> unit

  (** Get an element from the queue. The boolean shall be true to atomically
      start a new pledge (cf make_pledge) while popping. *)
  val pop : 'a t -> bool -> 'a option

  (** Make a new pledge, ie indicate that new elements may be pushed to the
      queue and that calls to pop should block waitting for them. *)
  val make_pledge : 'a t -> unit

  (** End one pledge. *)
  val end_pledge : 'a t -> unit

  (** Mark the queue as failed: all threads trying to pop from it will get no
      element. *)
  val fail : 'a t -> unit

  (** Pop all elements from the queue in a lazy Seq.t, *)
  val read_as_seq : 'a t -> finalizer:(unit -> unit) -> 'a Seq.t

  val work_while : ('a -> (Prio.t * 'a -> unit) -> unit) -> 'a t -> unit
end
