(* Add a notion of State to the Schedulable monad. "Transformer without module functor" style. *)
module M = Scheduler.Schedulable

type ('a, 's) t = 's -> ('a * 's) M.t

let[@inline] run mxf st = mxf st

let[@inline] return x = fun st -> M.return (x, st)

let[@inline] lift (x : 'a M.t) : ('a, 's) t =
 fun (st : 's) ->
  let ( let+ ) = M.( let+ ) in
  let+ x in
  (x, st)

let[@inline] bind mx f =
 fun st ->
  let ( let* ) = M.( let* ) in
  let* x, new_st = run mx st in
  run (f x) new_st

let[@inline] ( let* ) mx f = bind mx f

let[@inline] map x f =
  let* x in
  return (f x)

let[@inline] ( let+ ) x f = map x f

let[@inline] liftF2 f x y = fun st -> f (run x st) (run y st)

let[@inline] with_state f = fun st -> M.return (f st)

let[@inline] modify_state f = fun st -> M.return ((), f st)

let[@inline] project_state (project_and_backup : 'st1 -> 'st2 * 'backup) restore
  other =
 fun st ->
  let ( let+ ) = M.( let+ ) in
  let proj, backup = project_and_backup st in
  let+ res, new_state = run other proj in
  (res, restore backup new_state)
