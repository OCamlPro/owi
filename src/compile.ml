let ( let* ) o f = match o with Error msg -> Error msg | Ok v -> f v

let until_check m = Check.module_ m

let until_simplify m =
  let* () = until_check m in
  Simplify.modul m

let until_typecheck m =
  let* m = until_simplify m in
  let* () = Typecheck.module_ m in
  Ok m

let until_link link_state m =
  let* m = until_typecheck m in
  Link.modul m link_state

let until_interpret link_state m =
  let* m, link_state = until_link link_state m in
  let* () = Interpret.module_ m in
  Ok link_state
