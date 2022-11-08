let enable, disable, is_enabled =
  let debug_on = ref false in
  ( (fun () -> debug_on := true)
  , (fun () -> debug_on := false)
  , fun () -> !debug_on = true )

let log t =
  if is_enabled () then Format.eprintf t
  else Format.ifprintf Format.err_formatter t

let error f = Format.kasprintf failwith f
