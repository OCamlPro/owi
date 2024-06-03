(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type 'a t =
  { mutex : Mutex.t
  ; cond : Condition.t
  ; queue : 'a Queue.t
  ; mutable pledges : int
  ; mutable failed : bool
  }

let pop q pledge =
  Mutex.lock q.mutex;
  let r =
    try
      while Queue.is_empty q.queue do
        if q.pledges = 0 || q.failed then raise Exit;
        Condition.wait q.cond q.mutex
      done;
      let v = Queue.pop q.queue in
      if pledge then q.pledges <- q.pledges + 1;
      Some v
    with Exit ->
      Condition.broadcast q.cond;
      None
  in
  Mutex.unlock q.mutex;
  r

let make_pledge q =
  Mutex.lock q.mutex;
  q.pledges <- q.pledges + 1;
  Mutex.unlock q.mutex

let end_pledge q =
  Mutex.lock q.mutex;
  q.pledges <- q.pledges - 1;
  Condition.broadcast q.cond;
  Mutex.unlock q.mutex

let rec read_as_seq (q : 'a t) ~finalizer : 'a Seq.t =
 fun () ->
  match pop q false with
  | None ->
    finalizer ();
    Nil
  | Some v -> Cons (v, read_as_seq q ~finalizer)

let push v q =
  Mutex.lock q.mutex;
  let was_empty = Queue.is_empty q.queue in
  Queue.push v q.queue;
  if was_empty then Condition.broadcast q.cond;
  Mutex.unlock q.mutex

let fail q =
  Mutex.lock q.mutex;
  q.failed <- true;
  Condition.broadcast q.cond;
  Mutex.unlock q.mutex

let init () =
  { mutex = Mutex.create ()
  ; cond = Condition.create ()
  ; queue = Queue.create ()
  ; pledges = 0
  ; failed = false
  }
