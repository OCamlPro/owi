(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

type t = { model : Concrete_value.t list }

let empty =
  let model = [] in
  { model }

let add_to_model v { model } =
  let model = v :: model in
  { model }

let model_is_empty { model } = List.is_empty model

let get_model { model } =
  (* we have to reverse the list because it is constructed in reverse order *)
  List.rev model
