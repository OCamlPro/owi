(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type 'a t = 'a Result.t

let return x = Ok x [@@inline]

let bind x f = Result.bind x f [@@inline]

let ( let* ) = bind

let map v f = Result.map f v [@@inline]

let ( let+ ) = map

let select b = Ok b [@@inline]

let select_i32 i = Ok i [@@inline]

let trap t = Error t

let run m = m

let get_pc () = return Smtml.Expr.Set.empty
