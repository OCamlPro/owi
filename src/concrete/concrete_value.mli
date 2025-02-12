(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

include
  Value_intf.T
    with type bool = Bool.t
     and type int32 = Int32.t
     and type int64 = Int64.t
     and type float32 = Float32.t
     and type float64 = Float64.t
     and type ref_value = V.ref_value
     and type t = V.t
