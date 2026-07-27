(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

type t =
  { abs_state : Abstract_state.t
  ; env : Env.Abstract.t
  }

let pp ppf { abs_state; _ } = Fmt.pf ppf "%a" Abstract_state.pp abs_state
