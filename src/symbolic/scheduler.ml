module Schedulable = struct
  (* A monad representing computation that can be cooperatively scheduled. Computations can stop, continue with a value and fork. *)
  type 'a t =
    | Stop
    | Now of 'a
    | Fork of ('a t * (Prio.metrics * (unit -> 'a t)))

  let[@inline] return x : _ t = Now x

  let rec bind (mx : 'a t) (f : 'a -> 'b t) : 'b t =
    match mx with
    | Stop -> Stop
    | Now v -> f v
    | Fork (v, (prio, next)) ->
      Fork (bind v f, (prio, fun () -> bind (next ()) f))

  let[@inline] ( let* ) mx f = bind mx f

  let[@inline] map f x =
    let* x in
    return (f x)

  let[@inline] ( let+ ) x f = map f x

  let[@inline] fork v (prio, next) =
    Fork (return v, (prio, fun () -> return next))

  let stop = Stop
end

module Make (Work_datastructure : Prio.S) = struct
  (* A scheduler for Schedulable values. *)
  type 'a work_queue = (unit -> 'a Schedulable.t) Work_datastructure.t

  type 'a t = { work_queue : 'a work_queue } [@@unboxed]

  let[@inline] init () =
    let work_queue = Work_datastructure.make () in
    { work_queue }

  let[@inline] add_init_task sched task =
    Work_datastructure.push task Prio.dummy sched.work_queue

  let work sched at_worker_value =
    let rec inner (t : _ Schedulable.t) write_back : unit =
      match t with
      | Stop -> ()
      | Now x -> at_worker_value x
      | Fork (v, (prio, next)) ->
        (* TODO: should we change the order here? *)
        write_back (prio, next);
        inner v write_back
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
