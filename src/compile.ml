let ( let* ) o f = match o with Error msg -> Error msg | Ok v -> f v

let until_check m = Check.modul m

let until_simplify m =
  let* m = until_check m in
  let* m = Simplify.modul m in
  Ok m

let until_typecheck m =
  let* m = until_simplify m in
  let* () = Typecheck.modul m in
  Ok m

let until_optimize ~optimize m =
  let* m = until_typecheck m in
  if optimize then Ok (Optimize.modul m) else Ok m

let until_link link_state ~optimize ~name m =
  let* m = until_optimize ~optimize m in
  Link.modul link_state ~name m

let until_interpret link_state ~optimize ~name m =
  let* m, link_state = until_link link_state ~optimize ~name m in
  let* () = Interpret.modul m in
  Ok link_state
