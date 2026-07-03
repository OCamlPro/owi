(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

type 'a t = Abstract_state.t -> ('a * Abstract_state.t) option

let return x = fun state -> Some (x, state)

let bind f m = fun state -> Option.bind (m state) (fun (x, state) -> f x state)

let map f m = fun state -> Option.map (fun (x, state) -> (f x, state)) (m state)

let ( let* ) m f = bind f m

let ( let+ ) m f = map f m

let map_state f =
 fun state ->
  let state = f state in
  Option.map (fun state -> ((), state)) state

let fold_state f = fun state -> Some (f state, state)

let run m state = m state
