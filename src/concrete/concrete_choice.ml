(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type 'a t = 'a Result.t

let return x = Ok x [@@inline]

let bind x f = Result.bind x f [@@inline]

let ( let* ) x f = bind x f [@@inline]

let map v f = Result.map f v [@@inline]

let ( let+ ) v f = map v f [@@inline]

let select b ~prio_true:_ ~prio_false:_ = Ok b [@@inline]

let select_i32 i = Ok i [@@inline]

let trap t = Error t

let run m = m

let get_pc () = return Smtml.Expr.Set.empty

let depth () =
  (* TODO: maybe implement it at some point, it is only used in symbolic mode for exploration priority so we use 0 here for now *)
  Ok 0

let assume v _instr_counter =
  if v then Ok ()
  else
    (* TODO: there could be a dedicated error here? *)
    assert false

let ite cond ~if_true ~if_false =
  if cond then return if_true else return if_false
[@@inline]
