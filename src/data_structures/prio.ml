type t =
  | Val of
      { instr_counter : int
      ; distances_to_unreachable : int list
      }
  | Default
  | Random

let random = Random

let default = Default

let from_annotated instr_counter distances =
  let instr_counter = Atomic.get instr_counter in
  let distances_to_unreachable = List.sort compare (Array.to_list distances) in
  Val { instr_counter; distances_to_unreachable }

let compare p1 p2 =
  match (p1, p2) with
  | Val v1, Val v2 ->
    let c =
      match (v1.distances_to_unreachable, v2.distances_to_unreachable) with
      | [], [] -> 0
      | [], _ -> 1
      | _, [] -> -1
      | l1, l2 -> List.compare compare l1 l2
    in
    if c <> 0 then c else compare v1.instr_counter v2.instr_counter
  | Default, Default -> 0
  | Default, _ -> -1
  | _, Default -> 1
  | _ -> 0 (* à modifier *)
