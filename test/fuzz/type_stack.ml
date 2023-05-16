open Owi.Types.Symbolic

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
