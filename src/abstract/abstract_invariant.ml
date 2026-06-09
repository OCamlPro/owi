(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

type t = { cant_divide_by_zero : (int, unit) Hashtbl.t }

let empty () =
  let cant_divide_by_zero = Hashtbl.create 512 in
  { cant_divide_by_zero }

let cant_divide_by_zero { cant_divide_by_zero; _ } uuid =
  Hashtbl.mem cant_divide_by_zero uuid

let add_cant_divide_by_zero { cant_divide_by_zero; _ } uuid =
  Hashtbl.add cant_divide_by_zero uuid ()
