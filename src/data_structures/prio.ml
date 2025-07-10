type t =
  | Random
  | Val of int

let random = Random

let default = Val Int.max_int

let compute ~instr_counter = Val instr_counter

let to_int = function Random -> Random.int 10000000 | Val n -> n
