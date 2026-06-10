(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

module Int_map = Map.Make (Int)

type t = { can_divide_by_zero : bool Int_map.t }

let empty () =
  let can_divide_by_zero = Int_map.empty in
  { can_divide_by_zero }

let can_divide_by_zero { can_divide_by_zero } ~uuid =
  let possible = Int_map.find_opt uuid can_divide_by_zero in
  (* by default, we don't know, and assume it is possible to be on the safe-side *)
  Option.value ~default:true possible

let add_divide_by_zero_invariant ({ can_divide_by_zero } as invariant) ~uuid
  ~possible =
  match Int_map.find_opt uuid can_divide_by_zero with
  | None | Some false ->
    (* we had no information or it was impossible  *)
    if possible then begin
      (* it is now possible, thus we update with the new information *)
      let can_divide_by_zero = Int_map.add uuid possible can_divide_by_zero in
      { can_divide_by_zero }
    end
    else invariant
  | Some true ->
    (* if it was possible at some point, we never mark it as impossible *)
    invariant
