(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

module IntMap := Map.Make(Int)

type t =
  { globals : int IntMap.t
  ; memories : int IntMap.t
  ; elems : int IntMap.t
  ; datas : int IntMap.t
  ; tables : int IntMap.t
  ; functions : int IntMap.t
  ; tags : int IntMap.t
  ; type_base_id : int
  }

val empty : t

val rewrite_sub_type : map:t -> Binary.sub_type -> Binary.sub_type

val rewrite_type_id : map:t -> int -> int

val rewrite_binary_func : map:t -> Binary.Func.t -> _ Kind.func

val rewrite_expression :
  map:t -> Binary.expr Annotated.t -> Binary.expr Annotated.t
