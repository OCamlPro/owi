(* Add a notion of State to the Schedulable monad. "Transformer without module functor" style. *)
module M = Scheduler.Schedulable

(* TODO:
  we could use a CPS version of the state monad which could be much more efficient in hot paths, something like:

  type ('a, s') state =
    's -> ('a -> 's -> 'r) -> 'r

  let return x =
    fun s k ->
      k x s

  let bind m f =
    fun s k ->
      m s (fun x s -> f x s k)
  *)
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
