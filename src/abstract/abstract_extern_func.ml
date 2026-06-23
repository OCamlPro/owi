(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

include
  Extern.Func.Make
    (Abstract_value)
    (struct
      type 'a t = 'a
    end)
    (Abstract_memory)
    (Abstract_domain.Context)
