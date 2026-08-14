(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

module Key = struct
  type t =
    | I of int
    | Ret

  let map : (int -> int) -> t -> t =
   fun f key -> match key with I i -> I (f i) | Ret -> Ret

  let decr = map Int.pred

  let to_int = function I i -> i | Ret -> -1

  let pp fmt = function I i -> Fmt.pf fmt "%i" i | Ret -> Fmt.pf fmt "ret"
end

include PatriciaTree.MakeMap (Key)

type nonrec t = Abstract_interpreter_state.t list t

let append v1 v2 = idempotent_union (fun _ v1 v2 -> List.append v1 v2) v1 v2

let decr map =
  fold
    (fun k v acc -> match k with I 0 -> acc | k -> add (Key.decr k) v acc)
    map empty

let pp fmt =
  let pp_v = Fmt.list ~sep:Fmt.semi Abstract_interpreter_state.pp in
  pretty (fun fmt jk v -> Fmt.pf fmt "%a -> %a" Key.pp jk pp_v v) fmt
