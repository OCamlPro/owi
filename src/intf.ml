(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021 Léo Andrès *)
(* Copyright © 2021 Pierre Chambart *)

module type V = sig
  type int32

  type int64

  type float32

  type float64

  type vbool
end

module Concrete :
  V
    with type int32 = Int32.t
     and type int64 = Int64.t
     and type float32 = Float32.t
     and type float64 = Float64.t
     and type vbool = Bool.t = struct
  type int32 = Int32.t

  type int64 = Int64.t

  type float32 = Float32.t

  type float64 = Float64.t

  type vbool = Bool.t
end
