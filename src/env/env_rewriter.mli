(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

type t =
  { globals : Allocator.key Map.Make(Int).t
  ; memories : Allocator.key Map.Make(Int).t
  ; elems : Allocator.key Map.Make(Int).t
  ; datas : Allocator.key Map.Make(Int).t
  ; tables : Allocator.key Map.Make(Int).t
  ; functions : Allocator.key Map.Make(Int).t
  ; type_base_id : int
  }

val empty : t

val rewrite_sub_type : map:t -> Binary.sub_type -> Binary.sub_type

val rewrite_type_id : map:t -> int -> int

val rewrite_binary_func : map:t -> Binary.Func.t -> _ Kind.func

val rewrite_expression :
  map:t -> Binary.expr Annotated.t -> Binary.expr Annotated.t
