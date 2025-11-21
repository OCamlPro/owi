(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type 'a t =
  { index : int
  ; value : 'a
  }

let get v = v.value

let get_index v = v.index

let map f v = { index = v.index; value = f v.value }

let monadic_map f v =
  let open Syntax in
  let+ value = f v.value in
  { index = v.index; value }

let return index value = { index; value }

let has_index idx { index; _ } = idx = index

let get_at i values =
  match List.find_opt (has_index i) values with
  | None -> None
  | Some { value; _ } -> Some value

let sort l =
  List.sort
    (fun { index = i1; value = _ } { index = i2; value = _ } ->
      Int.compare i1 i2 )
    l

let list_to_array l = sort l |> List.map get |> Array.of_list

let list_to_dynarray l = sort l |> List.map get |> Dynarray.of_list

let pp pp_v fmt { index; value } =
  Fmt.pf fmt "{ index = %d ; value = %a }" index pp_v value

let pp_list pp_v fmt l =
  let pp fmt v = pp pp_v fmt v in
  Fmt.pf fmt "[%a]" (Fmt.list ~sep:(fun fmt () -> Fmt.pf fmt " ; ") pp) l
