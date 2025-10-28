(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type 'a t = ('a, Prio.t * 'a) Synchronizer.t

let pop q ~pledge = Synchronizer.get q ~pledge

let make_pledge = Synchronizer.make_pledge

let end_pledge = Synchronizer.end_pledge

let rec read_as_seq (q : 'a t) ~finalizer : 'a Seq.t =
 fun () ->
  match pop q ~pledge:false with
  | None ->
    finalizer ();
    Nil
  | Some v -> Cons (v, read_as_seq q ~finalizer)

let push v prio q = Synchronizer.write (prio, v) q

let work_while f q = Synchronizer.work_while f q

let close = Synchronizer.close

let make () =
  let q = Pq_imperative.empty () in
  let writter prio_v condvar =
    let prio, v = prio_v in
    Pq_imperative.push (Prio.to_int prio, v) q;
    Condition.signal condvar
  in
  Synchronizer.init (fun () -> Pq_imperative.pop q) writter
