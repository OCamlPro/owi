(* Add a notion of State to the Schedulable monad. "Transformer without module functor" style. *)
module M = Scheduler.Schedulable

type ('a, 's) t = St of ('s -> ('a * 's) M.t) [@@unboxed]

let[@inline] run (St mxf) st = mxf st

let[@inline] return x = St (fun st -> M.return (x, st))

let[@inline] lift (x : 'a M.t) : ('a, 's) t =
  let ( let+ ) = M.( let+ ) in
  St
    (fun (st : 's) ->
      let+ x in
      (x, st) )

let[@inline] bind mx f =
  St
    (fun st ->
      let ( let* ) = M.( let* ) in
      let* x, new_st = run mx st in
      run (f x) new_st )

let ( let* ) = bind

let[@inline] map x f =
  let* x in
  return (f x)

let[@inline] liftF2 f x y = St (fun st -> f (run x st) (run y st))

let ( let+ ) = map

let[@inline] with_state f = St (fun st -> M.return (f st))

let[@inline] modify_state f = St (fun st -> M.return ((), f st))

let[@inline] project_state (project_and_backup : 'st1 -> 'st2 * 'backup) restore
  other =
  St
    (fun st ->
      let ( let+ ) = M.( let+ ) in
      let proj, backup = project_and_backup st in
      let+ res, new_state = run other proj in
      (res, restore backup new_state) )
