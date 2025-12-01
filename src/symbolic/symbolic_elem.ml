(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type t = Link_env.elem

let get (elem : t) i : Symbolic_value.Ref.t =
  match elem.value.(i) with Func f -> Func f | _ -> assert false

let size (elem : t) = Array.length elem.value
