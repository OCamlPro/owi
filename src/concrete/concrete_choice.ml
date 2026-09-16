(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

type 'a t = Concrete_state.t -> ('a * Concrete_state.t) Result.t

let[@inline] return x = fun state -> Ok (x, state)

let[@inline] ( let* ) v f =
 fun state -> Result.bind (v state) (fun (v, state) -> f v state)

let[@inline] ( let+ ) v f =
 fun state -> Result.map (fun (v, state) -> (f v, state)) (v state)

let[@inline] select b ~instr_counter_true:_ ~instr_counter_false:_ = return b

let[@inline] select_i32 i = return i

let[@inline] trap t = fun _state -> Error t

let[@inline] run m state = m state

let[@inline] get_pc () = return Smtml.Expr.Set.empty

let[@inline] abort () =
  (* TODO: handle this properly! *)
  assert false

let[@inline] assume v = if v then return () else abort ()

let[@inline] assume_no_check v =
  (* TODO: we are supposed not to check here, but we check anyway, it may detect some bugs. *)
  if v then return () else abort ()

let[@inline] ite cond ~if_true ~if_false =
  if cond then return if_true else return if_false
