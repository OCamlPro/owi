(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

module Stack = Prelude.Stack

type 'a t = ('a, Prio.t * 'a) Synchronizer.t

let pop s ~pledge = Synchronizer.get s ~pledge

let new_pledge = Synchronizer.new_pledge

let end_pledge = Synchronizer.end_pledge

let rec read_as_seq (s : 'a t) ~finalizer : 'a Seq.t =
 fun () ->
  match pop s ~pledge:false with
  | None ->
    finalizer ();
    Nil
  | Some v -> Cons (v, read_as_seq s ~finalizer)

let push v prio s = Synchronizer.write s (prio, v)

let work_while f s = Synchronizer.work_while f s

let close = Synchronizer.close

let make () =
  let s = Stack.create () in
  let writter prio_v = Stack.push (snd prio_v) s in
  Synchronizer.init (fun () -> Stack.pop_opt s) writter
