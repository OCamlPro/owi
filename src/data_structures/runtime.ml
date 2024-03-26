(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021 Léo Andrès *)
(* Copyright © 2021 Pierre Chambart *)

type ('a, 'b) t =
  | Local of 'a
  | Imported of 'b Imported.t
