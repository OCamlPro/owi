type t =
  | Val of
      { instr_counter : int
      ; distances_to_unreachable : int list option
      }
  | Default
  | Random

let random = Random

let default = Default

let from_annotated instr_counter distances =
  let instr_counter = Atomic.get instr_counter in
  let distances_to_unreachable =
    Option.bind distances (fun d -> Some (List.sort compare (Array.to_list d)))
  in
  Val { instr_counter; distances_to_unreachable }

let compare p1 p2 =
  match (p1, p2) with
  | Val v1, Val v2 ->
    if Int.abs (v1.instr_counter - v2.instr_counter) > 5 then compare v1.instr_counter v2.instr_counter else
     ( match (v1.distances_to_unreachable, v2.distances_to_unreachable) with
      | None, None -> 0
      | None, _ -> 1
      | _, None -> -1
      | Some l1, Some l2 -> if Int.abs (v1.instr_counter - v2.instr_counter) > 5 then compare v1.instr_counter v2.instr_counter else List.compare compare l1 l2
     )
  | Default, Default -> 0
  | Default, _ -> -1
  | _, Default -> 1
  | _ -> 0 (* à modifier *)
