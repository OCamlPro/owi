(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

include Map.Make (Int)

let next_key map = cardinal map

let add_manual k v map = add k v map

let add v map =
  let key = next_key map in
  let map = add key v map in
  (map, key)

let pp pp_v =
  Fmt.braces
    (Fmt.iter_bindings ~sep:Fmt.semi iter (fun ppf (k, v) ->
       Fmt.pf ppf "%d -> %a" k pp_v v ) )
