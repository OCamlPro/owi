(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type ('get, 'write) t =
  { mutex : Mutex.t
  ; cond : Condition.t
  ; getter : unit -> 'get option
  ; writter : 'write -> int -> Condition.t -> unit
  ; mutable pledges : int
  ; mutable closed : bool
  }

let init getter writter =
  { mutex = Mutex.create ()
  ; cond = Condition.create ()
  ; getter
  ; writter
  ; pledges = 0
  ; closed = false
  }

let get synchro pledge =
  let rec inner_loop synchro pledge =
    match synchro.getter () with
    | None when synchro.pledges = 0 || synchro.closed ->
      Condition.broadcast synchro.cond;
      None
    | None ->
      Condition.wait synchro.cond synchro.mutex;
      inner_loop synchro pledge
    | Some _ as v ->
      if pledge then synchro.pledges <- synchro.pledges + 1;
      v
  in
  Mutex.protect synchro.mutex (fun () -> inner_loop synchro pledge)

let write v prio { writter; cond; mutex; _ } =
  Mutex.protect mutex (fun () -> writter v prio cond)

let make_pledge synchro =
  Mutex.lock synchro.mutex;
  synchro.pledges <- synchro.pledges + 1;
  Mutex.unlock synchro.mutex

let end_pledge synchro =
  Mutex.lock synchro.mutex;
  synchro.pledges <- synchro.pledges - 1;
  if Int.equal synchro.pledges 0 then Condition.broadcast synchro.cond;
  Mutex.unlock synchro.mutex

let fail q =
  Mutex.lock q.mutex;
  q.closed <- true;
  Condition.broadcast q.cond;
  Mutex.unlock q.mutex

let rec work_while f q =
  match get q true with
  | None -> ()
  | Some v ->
    let () = f v (fun v prio -> write v prio q) in
    end_pledge q;
    work_while f q
