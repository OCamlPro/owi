type t =
  | Out_of_bounds_table_access
  | Out_of_bounds_memory_access
  | Integer_overflow
  | Integer_divide_by_zero
  | Unreachable

let to_string = function
  | Out_of_bounds_table_access -> "out of bounds table access"
  | Out_of_bounds_memory_access -> "out of bounds memory access"
  | Integer_overflow -> "integer overflow"
  | Integer_divide_by_zero -> "integer divide by zero"
  | Unreachable -> "unreachable"
