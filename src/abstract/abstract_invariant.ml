(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

type t = { cant_divide_by_zero : (int, bool) Hashtbl.t }

let empty () =
  let cant_divide_by_zero = Hashtbl.create 512 in
  { cant_divide_by_zero }

let cant_divide_by_zero { cant_divide_by_zero; _ } uuid =
  match Hashtbl.find_opt cant_divide_by_zero uuid with
  | Some true -> true
  | _ -> false

let add_cant_divide_by_zero { cant_divide_by_zero; _ } uuid v =
  match Hashtbl.find_opt cant_divide_by_zero uuid with
  | Some false -> ()
  | Some true -> Hashtbl.replace cant_divide_by_zero uuid v
  | None -> Hashtbl.add cant_divide_by_zero uuid v
