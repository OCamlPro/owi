(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021 Léo Andrès *)
(* Copyright © 2021 Pierre Chambart *)

(** the types of imported values *)
type 'a t =
  { modul : string
  ; name : string
  ; assigned_name : string option
  ; desc : 'a
  }
