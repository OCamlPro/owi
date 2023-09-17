(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021 Léo Andrès *)
(* Copyright © 2021 Pierre Chambart *)

let ( let* ) o f = match o with Error msg -> Error msg | Ok v -> f v

let until_check m = Check.modul m

let until_group m =
  let* m = until_check m in
  let* m = Grouped.of_symbolic m in
  Ok m

let until_assign m =
  let* m = until_group m in
  let* m = Assigned.of_grouped m in
  Ok m

let until_simplify m =
  let* m = until_assign m in
  let* m = Rewrite.modul m in
  Ok m

let until_typecheck ?(unsafe = false) m =
  let* m = until_simplify m in
  if unsafe then Ok m
  else
    let* () = Typecheck.modul m in
    Ok m

let until_optimize ?unsafe ~optimize m =
  let* m = until_typecheck ?unsafe m in
  if optimize then Ok (Optimize.modul m) else Ok m

let until_link ?unsafe link_state ~optimize ~name m =
  let* m = until_optimize ?unsafe ~optimize m in
  Link.modul link_state ~name m

let until_interpret link_state ~optimize ~name m =
  let* m, link_state = until_link link_state ~optimize ~name m in
  let* () = Interpret.I.modul link_state.envs m in
  Ok link_state
