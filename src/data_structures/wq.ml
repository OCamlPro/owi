(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type 'a t = ('a, 'a) Synchronizer.t

let pop q pledge = Synchronizer.get q pledge

let make_pledge = Synchronizer.make_pledge

let end_pledge = Synchronizer.end_pledge

let rec read_as_seq (q : 'a t) ~finalizer : 'a Seq.t =
 fun () ->
  match pop q false with
  | None ->
    finalizer ();
    Nil
  | Some v -> Cons (v, read_as_seq q ~finalizer)

let push v q = Synchronizer.write v q

let work_while f q = Synchronizer.work_while f q

let fail = Synchronizer.fail

(*let make () =
  let mutex = Mutex.create () in
  let q = ref Priority_queue.empty in
  let writter v condvar =
    Mutex.protect mutex (fun () ->
      let q' = Priority_queue.push v !q in
      q := q' );
    Condition.signal condvar
  in
  Synchronizer.init
    (fun () ->
      Mutex.protect mutex (fun () ->
        let x, q' = Priority_queue.pop !q in
        q := q';
        x ) )
    writter*)

let make () =
  let mutex = Mutex.create () in
  let q = Pq_imperative.empty () in
  let writter v condvar =
    Mutex.protect mutex (fun () -> Pq_imperative.push v q);
    Condition.signal condvar
  in
  Synchronizer.init
    (fun () -> Mutex.protect mutex (fun () -> Pq_imperative.pop q))
    writter
