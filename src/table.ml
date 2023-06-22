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

let get t i = t.data.(Int32.to_int i)
let set t i v = t.data.(Int32.to_int i) <- v
let size t = Int32.of_int @@ Array.length t.data
