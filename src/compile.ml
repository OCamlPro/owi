(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax

let until_check ~unsafe m = if unsafe then Ok m else Check.modul m

let until_group ~unsafe m =
  let* m = until_check ~unsafe m in
  Grouped.of_symbolic m

let until_assign ~unsafe m =
  let* m = until_group ~unsafe m in
  Assigned.of_grouped m

let until_simplify ~unsafe m =
  let* m = until_assign ~unsafe m in
  Rewrite.modul m

let until_typecheck ~unsafe m =
  let* m = until_simplify ~unsafe m in
  if unsafe then Ok m
  else
    let+ () = Typecheck.modul m in
    m

let until_optimize ~unsafe ~optimize m =
  let+ m = until_typecheck ~unsafe m in
  if optimize then Optimize.modul m else m

let until_link ~unsafe link_state ~optimize ~name m =
  let* m = until_optimize ~unsafe ~optimize m in
  Link.modul link_state ~name m

let until_interpret link_state ~unsafe ~optimize ~name m =
  let* m, link_state = until_link link_state ~unsafe ~optimize ~name m in
  let+ () = Interpret.Concrete.modul link_state.envs m in
  link_state
