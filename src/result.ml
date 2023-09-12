(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021 Léo Andrès *)
(* Copyright © 2021 Pierre Chambart *)

include Stdlib.Result

type 'a t = ('a, string) Stdlib.Result.t
