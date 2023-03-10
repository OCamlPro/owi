let debug_on = ref false

let debug t =
  if !debug_on then Format.eprintf t else Format.ifprintf Format.err_formatter t

let debug0 t : unit =
  if !debug_on then Format.eprintf t

let debug1 t a : unit =
  if !debug_on then Format.eprintf t a

let debug2 t a b : unit =
  if !debug_on then Format.eprintf t a b

let err f = Format.kasprintf failwith f
