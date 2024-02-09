(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021 Léo Andrès *)
(* Copyright © 2021 Pierre Chambart *)

type 'a t = 'a

let return x = x [@@inline]

let bind x f = f x [@@inline]

let ( let* ) = bind

let map v f =
  let* v in
  return (f v)
[@@inline]

let ( let+ ) = map

let select b = b [@@inline]

let select_i32 i = i [@@inline]

let get = ()

let trap msg = raise (Types.Trap msg)

let trap : Trap.t -> 'a t = fun tr -> trap (Trap.to_string tr)
