type t =
  | Out_of_bounds_table_access
  | Out_of_bounds_memory_access
  | Undefined_element
  | Uninitialized_element of int
  | Integer_overflow
  | Integer_divide_by_zero
  | Element_type_error
  | Unreachable
  | Indirect_call_type_mismatch

let to_string = function
  | Out_of_bounds_table_access -> "out of bounds table access"
  | Out_of_bounds_memory_access -> "out of bounds memory access"
  | Undefined_element -> "undefined element"
  | Uninitialized_element fun_i -> Printf.sprintf "uninitialized element %i" fun_i
  | Integer_overflow -> "integer overflow"
  | Integer_divide_by_zero -> "integer divide by zero"
  | Element_type_error -> "element_type_error"
  | Unreachable -> "unreachable"
  | Indirect_call_type_mismatch -> "indirect call type mismatch"
