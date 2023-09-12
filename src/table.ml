(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021 Léo Andrès *)
(* Copyright © 2021 Pierre Chambart *)

open Types
open Simplified

(* TODO: Value.ref_value array, gadt to constraint to the right ref_type ? *)
type table = Value.ref_value array

type t =
  { id : int
  ; label : string option
  ; limits : limits
  ; typ : ref_type
  ; mutable data : table
  }

let fresh =
  let r = ref (-1) in
  fun () ->
    incr r;
    !r

let init ?label (typ : table_type) : t =
  let limits, ((_null, heap_type) as ref_type) = typ in
  let null = Value.ref_null' heap_type in
  let table = Array.make limits.min null in
  { id = fresh (); label; limits; typ = ref_type; data = table }

let update table data = table.data <- data

let get t i = t.data.(i)

let set t i v = t.data.(i) <- v

let size t = Array.length t.data

let typ t = t.typ