module Make (Work_datastructure : Prio.S) = struct
  (* A scheduler for Schedulable values. *)
  type 'a work_queue =
    (unit -> ('a, Prio.metrics) Symex.Monad.Schedulable.t) Work_datastructure.t

  type 'a t = { work_queue : 'a work_queue } [@@unboxed]

  let[@inline] init () =
    let work_queue = Work_datastructure.make () in
    { work_queue }

  let[@inline] add_init_task sched task =
    Work_datastructure.push task Prio.dummy sched.work_queue

  let work sched at_worker_value =
    let rec inner (t : _ Symex.Monad.Schedulable.t) write_back =
      match t with
      | Prune -> ()
      | Now x -> at_worker_value x
      | Yield (prio, f) -> write_back (prio, f)
      | Choice (m1, m2) ->
        inner m1 write_back;
        inner m2 write_back
    in
    Work_datastructure.work_while
      (fun f write_back -> inner (f ()) write_back)
      sched.work_queue

  let run_worker sched
    ~(at_worker_value : close_work_queue:(unit -> unit) -> 'a -> unit) ~finally
      =
    Fun.protect ~finally (fun () ->
      try
        work sched
          (at_worker_value ~close_work_queue:(fun () ->
             Work_datastructure.close sched.work_queue ) )
      with e ->
        let e_s = Printexc.to_string e in
        let bt = Printexc.get_raw_backtrace () in
        let bt_s = Printexc.raw_backtrace_to_string bt in
        let bt_s =
          if String.equal "" bt_s then
            "use OCAMLRUNPARAM=b to get the backtrace"
          else bt_s
        in
        Log.err (fun m ->
          m "a worker ended with exception %s, backtrace is: @\n@[<v>%s@]" e_s
            bt_s );
        Printexc.raise_with_backtrace e bt )
end
