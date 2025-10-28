type 'a t =
  | Empty
  | Node of 'a t * int * 'a * 'a t

let empty = Empty

let is_empty h = match h with Empty -> true | _ -> false

let rec merge h1 h2 =
  match (h1, h2) with
  | Empty, h | h, Empty -> h
  | Node (l1, k1, v1, r1), Node (l2, k2, v2, r2) ->
    if compare k1 k2 <= 0 then Node (r1, k1, v1, merge l1 h2)
    else Node (r2, k2, v2, merge l2 h1)

let pop h =
  match h with Empty -> (None, h) | Node (l, _, v, r) -> (Some v, merge l r)

let push v h = merge h (Node (Empty, 1, v, Empty))
