let ( let* ) o f = match o with Error msg -> Error msg | Ok v -> f v

let until_check m = Check.module_ m

let until_simplify ~optimize m =
  let* () = until_check m in
  let* m = Simplify.modul m in
  if optimize then
  Ok (Optimize.modul m)
  else Ok m

let until_typecheck ~optimize m =
  let* m = until_simplify ~optimize m in
  let* () = Typecheck.module_ m in
  Ok m

let until_link link_state ~optimize ~name m =
  let* m = until_typecheck ~optimize m in
  Link.modul link_state ~name m

let until_interpret link_state ~optimize ~name m =
  let* m, link_state = until_link link_state ~optimize ~name m in
  let* () = Interpret.module_ m in
  Ok link_state
