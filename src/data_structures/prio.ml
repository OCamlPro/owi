type t =
  | Random
  | Val of int

let random = Random

let default = Val 0

let of_int n = Val n

let to_int = function Random -> Random.int 10000000 | Val n -> n
