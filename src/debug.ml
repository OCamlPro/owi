let enable, disable, is_enabled =
  let debug_on = ref false in
  ( (fun () -> debug_on := true)
  , (fun () -> debug_on := false)
  , fun () -> !debug_on = true )

let debug fmt =
  if is_enabled () then
    Format.fprintf fmt
  else
    Format.ifprintf fmt
