(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

(** the types of imported values *)
type 'a t =
  { modul : string
  ; name : string
  ; assigned_name : string option
  ; typ : 'a
  }

let pp pp_typ fmt { modul; name; assigned_name; typ } =
  Fmt.pf fmt "{@\n  @[<v>modul: %S@\nname: %S@\nassigned_name: %a@\ntyp: %a@]}"
    modul name Text.pp_id_opt assigned_name pp_typ typ
