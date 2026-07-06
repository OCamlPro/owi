(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

type 'a t = Abstract_state.t -> 'a * Abstract_state.t

let return (x : 'a) : 'a t = fun state -> (x, state)

let bind (m : 'a t) (f : 'a -> 'b t) : 'b t =
 fun state ->
  let x, state' = m state in
  f x state'

let map (f : 'a -> 'b) (m : 'a t) : 'b t =
 fun state ->
  let x, state' = m state in
  (f x, state')

let ( let* ) = bind

let ( let+ ) f a = map a f

let get_state : Abstract_state.t t = fun state -> (state, state)

let set_state (state : Abstract_state.t) : unit t = fun _ -> ((), state)

let modify_state (f : Abstract_state.t -> Abstract_state.t) : unit t =
 fun state -> ((), f state)
