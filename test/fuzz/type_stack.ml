open Owi.Symbolic

type stack_op =
  | Pop
  | Push of val_type
  | Nothing

let apply_stack_op stack op =
  match op with
  | Pop -> begin match stack with [] -> assert false | _hd :: tl -> tl end
  | Push t -> t :: stack
  | Nothing -> stack

let apply_stack_ops stack ops = List.fold_left apply_stack_op stack ops

let rec is_stack_compatible st1 st2 =
  match (st1, st2) with
  | _, [] -> true
  | [], _ -> false
  | s1 :: st1, s2 :: st2 -> s1 = s2 && is_stack_compatible st1 st2

let is_stack_compatible_param stack pt =
  let s = List.map (fun p -> snd p) pt in
  is_stack_compatible stack s
