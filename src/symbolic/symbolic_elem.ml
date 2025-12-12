(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type t = Concrete_elem.t

let get (elem : t) i : Symbolic_ref.t =
  match elem.value.(i) with Func f -> Func f | _ -> assert false

let size (elem : t) = Array.length elem.value

let drop (e : t) = e.value <- [||]
