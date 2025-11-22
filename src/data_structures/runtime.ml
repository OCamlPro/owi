(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type 'a imported =
  { modul_name : string
  ; name : string
  ; assigned_name : string option
  ; typ : 'a
  }

type ('a, 'b) t =
  | Local of 'a
  | Imported of 'b imported

let imported ~modul_name ~name ~assigned_name ~typ =
  Imported { modul_name; name; assigned_name; typ }

let pp_imported pp_typ fmt { modul_name; name; assigned_name; typ } =
  Fmt.pf fmt "{@\n  @[<v>modul: %S@\nname: %S@\nassigned_name: %a@\ntyp: %a@]}"
    modul_name name Text.pp_id_opt assigned_name pp_typ typ

let map_imported f v =
  let typ = f v.typ in
  { v with typ }

let monadic_map_imported f v =
  let open Syntax in
  let+ typ = f v.typ in
  { v with typ }

let pp ~pp_local ~pp_imported:pp fmt = function
  | Local local -> Fmt.pf fmt "Local (%a)" pp_local local
  | Imported imported -> Fmt.pf fmt "Imported (%a)" (pp_imported pp) imported

let map ~f_local ~f_imported = function
  | Local v ->
    let v = f_local v in
    Local v
  | Imported v ->
    let v = map_imported f_imported v in
    Imported v

let monadic_map ~f_local ~f_imported =
  let open Syntax in
  function
  | Local v ->
    let+ v = f_local v in
    Local v
  | Imported v ->
    let+ v = monadic_map_imported f_imported v in
    Imported v
