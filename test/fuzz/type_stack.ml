open Owi.Text

type elem = val_type

type stack_op =
  | Pop
  | Push of elem
  | Whatever of elem list
  | Nothing

let apply_stack_op stack op =
  match op with
  | Pop -> begin match stack with [] -> assert false | _hd :: tl -> tl end
  | Push t -> t :: stack
  | Nothing -> stack
  | Whatever stack -> stack

let apply_stack_ops stack ops = List.fold_left apply_stack_op stack ops

let rec is_stack_compatible st1 st2 =
  match (st1, st2) with
  | _, [] -> true
  | [], _ -> false
  | s1 :: st1, s2 :: st2 -> val_type_eq s1 s2 && is_stack_compatible st1 st2

let is_stack_compatible_param stack pt =
  let s = List.map snd pt in
  is_stack_compatible stack s
