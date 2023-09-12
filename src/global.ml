(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021 Léo Andrès *)
(* Copyright © 2021 Pierre Chambart *)

type 'env t =
  { mutable value : 'env Value.t
  ; label : string option
  ; mut : Types.mut
  ; typ : Simplified.val_type
  }
