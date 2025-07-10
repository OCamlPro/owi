type t =
  | Random
  | Val of int

let random = Random

let default = Val Int.max_int

let of_int n = Val n

let to_int = function Random -> Random.int 10000000 | Val n -> n
