(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

type t = { can_divide_by_zero : (int, bool) Hashtbl.t }

let empty () =
  let can_divide_by_zero = Hashtbl.create 512 in
  { can_divide_by_zero }

let can_divide_by_zero { can_divide_by_zero } ~uuid =
  let possible = Hashtbl.find_opt can_divide_by_zero uuid in
  (* by default, we don't know, and assume it is possible to be on the safe-side *)
  Option.value ~default:true possible

let add_divide_by_zero_invariant { can_divide_by_zero } ~uuid ~possible =
  match Hashtbl.find_opt can_divide_by_zero uuid with
  | None -> begin Hashtbl.add can_divide_by_zero uuid possible end
  | Some false ->
    if possible then begin
      (* it was impossible but it is now possible *)
      Hashtbl.add can_divide_by_zero uuid true
    end
    else begin
      (* it was impossible and it is still impossible *)
      ()
    end
  | Some true ->
    (* it was possible, so it must stay true *)
    ()

let pp_can_divide_by_zero fmt m =
  Fmt.iter_bindings ~sep:Fmt.semi Hashtbl.iter
    (fun ppf (k, v) -> Fmt.pf ppf "%d -> %b" k v)
    fmt m

let pp fmt { can_divide_by_zero } =
  Fmt.pf fmt "{ can_divide_by_zero : %a }" pp_can_divide_by_zero
    can_divide_by_zero
