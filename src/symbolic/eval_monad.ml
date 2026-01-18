(* Add a notion of faillibility to the evaluation. "Transformer without module functor" style. *)
module M = State_monad

type ('a, 's) t = (('a, Bug.t) result, 's) M.t

let[@inline] return x : _ t = M.return (Ok x)

let[@inline] lift x =
  let ( let+ ) = M.( let+ ) in
  let+ x in
  Ok x

let[@inline] bind (mx : _ t) f : _ t =
  let ( let* ) = M.( let* ) in
  let* mx in
  match mx with Ok x -> f x | Error _ as mx -> M.return mx

let[@inline] ( let* ) mx f = bind mx f

let[@inline] map mx f =
  let ( let+ ) = M.( let+ ) in
  let+ mx in
  match mx with Ok x -> Ok (f x) | Error _ as mx -> mx

let[@inline] ( let+ ) mx f = map mx f
