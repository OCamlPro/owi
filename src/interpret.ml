(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021 Léo Andrès *)
(* Copyright © 2021 Pierre Chambart *)

module I = Interpret_functor.Make (Value_test.P) [@@inlined hint]
module S = Interpret_functor.Make (Sym_state.P) [@@inlined hint]