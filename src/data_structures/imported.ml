(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

(** the types of imported values *)
type 'a t =
  { modul : string
  ; name : string
  ; assigned_name : string option
  ; desc : 'a
  }

let pp pp_desc fmt { modul; name; assigned_name; desc } =
  Fmt.pf fmt "{@\n  @[<v>modul: %S@\nname: %S@\nassigned_name: %a@\ndesc: %a@]}"
    modul name Text.pp_id_opt assigned_name pp_desc desc
