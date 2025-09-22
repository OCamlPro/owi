type t =
  | Val of
      { instr_counter : int
      ; distances_to_unreachable : int list option
      }
  | Default
  | Random of int

let random = Random (Random.int 10000)

let default = Default

let from_annotated instr_counter distances =
  let distances_to_unreachable =
    Option.bind distances (fun d -> Some (List.sort compare (Array.to_list d)))
  in
  Val { instr_counter; distances_to_unreachable }

let compare p1 p2 =
  match (p1, p2) with
  | Val v1, Val v2 -> (
    match (v1.distances_to_unreachable, v2.distances_to_unreachable) with
    | None, None -> 0
    | None, _ -> 1
    | _, None -> -1
    | Some l1, Some l2 ->
      if v1.instr_counter = v2.instr_counter then List.compare compare l1 l2
      else if v1.instr_counter = 0 then -1
      else if v2.instr_counter = 0 then 1
      else List.compare compare l1 l2 )
  | Default, Default -> 0
  | Default, _ -> -1
  | _, Default -> 1
  | Random x, Random y -> compare x y
  | _ -> assert false
