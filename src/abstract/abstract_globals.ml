(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

include PatriciaTree.MakeMap (struct
  include Int

  let to_int x = x
end)

let pp pp_v fmt =
  pretty ~pp_sep:(Fmt.any "; @;")
    (fun fmt k v -> Fmt.pf fmt "(%i -> %a)" k pp_v v)
    fmt
