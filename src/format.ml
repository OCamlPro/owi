include Stdlib.Format

let pp = fprintf

let pp_nothing _fmt () = ()

let pp_list = pp_print_list

let pp_array = pp_print_array

let pp_iter = pp_print_iter

let pp_string = pp_print_string

let pp_option = pp_print_option

let pp_bool = pp_print_bool

let pp_flush = pp_print_flush

let pp_space = pp_print_space

let pp_int = pp_print_int
