(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

module Int_pair = struct
  type t = int * int

  let compare (l1, r1) (l2, r2) =
    let res = compare l1 l2 in
    if l1 = l2 then compare r1 r2 else res
end

module Int_pair_map = Map.Make (Int_pair)

type 'a t = 'a Int_pair_map.t

let empty = Int_pair_map.empty

let find collection ~modul ~id =
  let loc = (modul, id) in
  Int_pair_map.find_opt loc collection

let replace collection ~modul ~id v =
  let loc = (modul, id) in
  Int_pair_map.add loc v collection
