(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

type 'a t = Concrete_state.t -> Concrete_state.t * 'a Result.t

let[@inline] return v = fun state -> (state, Ok v)

let[@inline] ( let* ) v f =
 fun state ->
  let state, result = v state in
  match result with Ok v -> f v state | Error e -> (state, Error e)

let[@inline] ( let+ ) v f =
 fun state ->
  let state, result = v state in
  match result with Ok v -> (state, Ok (f v)) | Error e -> (state, Error e)

let[@inline] select b ~instr_counter_true:_ ~instr_counter_false:_ = return b

let[@inline] select_i32 i = return i

let[@inline] trap t = fun state -> (state, Error t)

let[@inline] run m state =
  let state, outcome = m state in
  match outcome with Ok v -> Ok (state, v) | Error e -> Error (state, e)

let[@inline] run_and_drop_state m state =
  let _state, outcome = m state in
  outcome

let[@inline] get_pc () = return Smtml.Expr.Set.empty

let[@inline] abort () =
  (* TODO: handle this properly! *)
  assert false

let[@inline] assume v = if v then return () else abort ()

let[@inline] assume_no_check v =
  (* TODO: we are supposed not to check here, but we check anyway, it may detect some bugs. *)
  if v then return () else abort ()

let[@inline] map_state f =
 fun state ->
  let state = f state in
  (state, Ok ())

let[@inline] fold_state f = fun state -> (state, Ok (f state))

let[@inline] ite cond ~if_true ~if_false =
  if cond then return if_true else return if_false
