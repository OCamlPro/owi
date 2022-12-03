let debug_on = ref false

let debug t =
  if !debug_on then Format.eprintf t else Format.ifprintf Format.err_formatter t

let err f = Format.kasprintf failwith f
