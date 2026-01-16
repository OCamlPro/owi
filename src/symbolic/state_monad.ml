(* Add a notion of State to the Schedulable monad. "Transformer without module functor" style. *)
module M = Scheduler.Schedulable

type ('a, 's) t =
  | St of ('s -> ('a * 's) M.t)
  | Immediate of 'a M.t

let[@inline] run mxf st =
  match mxf with St mxf -> mxf st | Immediate v -> M.map v (fun v -> (v, st))

let[@inline] run_immediate mxf =
  match mxf with St _ -> None | Immediate v -> Some v

let[@inline] return x = Immediate (M.return x)

let[@inline] lift (x : 'a M.t) : ('a, 's) t =
  let ( let+ ) = M.( let+ ) in
  St
    (fun (st : 's) ->
      let+ x in
      (x, st) )

let[@inline] join (mmx : (('a, 's) t, 's) t) : ('a, 's) t =
  match mmx with
  | Immediate v ->
    let x = M.map v run_immediate in
    begin match M.try_commute_opt x with
    | Some v -> Immediate (M.join v)
    | None ->
      St
        (fun st ->
          let ( let* ) = M.( let* ) in
          let* mx = v in
          run mx st )
    end
  | St f ->
    St
      (fun st ->
        let ( let* ) = M.( let* ) in
        let* x, new_st = f st in
        run x new_st )

let[@inline] map x f =
  match x with
  | Immediate v -> Immediate (M.map v f)
  | St step ->
    St
      (fun st ->
        let ( let* ) = M.( let* ) in
        let* x, new_st = step st in
        M.return (f x, new_st) )

let[@inline] bind (mx : ('a, 's) t) (f : 'a -> ('b, 's) t) : ('b, 's) t =
  join (map mx f)

let ( let* ) = bind

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
