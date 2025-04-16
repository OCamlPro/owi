(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2025 OCamlPro *)
(* Written by the Owi programmers *)

include
  Func_intf.Make_extern_func
    (struct
      type int32 = Int32.t

      type int64 = Int64.t

      type float32 = Float32.t

      type float64 = Float64.t

      type bool = Bool.t
    end)
    (struct
      type 'a t = 'a Result.t
    end)
    (Concrete_memory)
