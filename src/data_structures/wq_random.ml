type 'a t = ('a, Prio.t * 'a) Synchronizer.t

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

let push v prio q = Synchronizer.write (prio, v) q

let work_while f q = Synchronizer.work_while f q

let fail = Synchronizer.fail

let make () =
  let q = Pq_imperative.empty () in
  Random.init 0;
  let writter prio_v condvar =
    Pq_imperative.push (Prio.to_int Prio.random, snd prio_v) q;
    Condition.signal condvar
  in
  Synchronizer.init (fun () -> Pq_imperative.pop q) writter
