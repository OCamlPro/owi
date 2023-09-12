(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021 Léo Andrès *)
(* Copyright © 2021 Pierre Chambart *)

module type Env = sig
  type t

  type memory

  type func

  type table

  type elem

  type data

  type global

  module Value : Value_intf.T
end
