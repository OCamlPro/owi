(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2025 OCamlPro *)
(* Written by the Owi programmers *)

include
  Extern.Func.Make
    (struct
      type int32 = Int32.t

      type int64 = Int64.t

      type float32 = Float32.t

      type float64 = Float64.t

      type v128 = V128.t
    end)
    (Result)
    (Concrete_memory)
