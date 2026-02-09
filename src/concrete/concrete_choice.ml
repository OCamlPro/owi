(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type 'a t = 'a Result.t

let[@inline] return x = Ok x

let[@inline] bind x f = Result.bind x f

let[@inline] ( let* ) x f = bind x f

let[@inline] map f v = Result.map f v

let[@inline] ( let+ ) v f = map f v

let[@inline] select b ~instr_counter_true:_ ~instr_counter_false:_ = Ok b

let[@inline] select_i32 i = Ok i

let[@inline] trap t = Error t

let[@inline] run m = m

let[@inline] get_pc () = return Smtml.Expr.Set.empty

let[@inline] assume v =
  if v then Ok ()
  else
    (* TODO: there could be a dedicated error here? *)
    assert false

let[@inline] ite cond ~if_true ~if_false =
  if cond then return if_true else return if_false
