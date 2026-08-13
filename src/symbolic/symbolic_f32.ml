(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

include Smtml.Typed.Float32

let of_float32 (f : Float32.t) : t = Smtml.Typed.Float32.v (Float32.to_bits f)

let of_bits x = Smtml.Typed.Float32.reinterpret_i32 x

let to_bits x = Smtml.Typed.Float32.to_bv x

let pmin _ = assert false

let pmax _ = assert false

let to_float32 (_ : t) : Float32.t =
  Fmt.failwith "TODO: unimplemented Symbolic_f32.to_float32"
