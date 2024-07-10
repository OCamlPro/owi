(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax

module Text = struct
  let until_check ~unsafe m = if unsafe then Ok m else Check.modul m

  let until_group ~unsafe m =
    let* m = until_check ~unsafe m in
    Grouped.of_symbolic m

  let until_assign ~unsafe m =
    let* m = until_group ~unsafe m in
    Assigned.of_grouped m

  let until_binary ~unsafe m =
    let* m = until_assign ~unsafe m in
    Rewrite.modul m

  let until_typecheck ~unsafe m =
    let* m = until_binary ~unsafe m in
    if unsafe then Ok m
    else
      let+ () = Typecheck.modul m in
      m

  let until_optimize ~unsafe ~optimize m =
    let+ m = until_typecheck ~unsafe m in
    if optimize then Optimize.modul m else m

  let until_link ~unsafe ~optimize ~name link_state m =
    let* m = until_optimize ~unsafe ~optimize m in
    Link.modul link_state ~name m

  let until_interpret ~unsafe ~optimize ~name link_state m =
    let* m, link_state = until_link ~unsafe ~optimize ~name link_state m in
    let+ () = Interpret.Concrete.modul link_state.envs m in
    link_state
end

module Binary = struct
  let until_typecheck ~unsafe m =
    if unsafe then Ok m
    else
      let+ () = Typecheck.modul m in
      m

  let until_optimize ~unsafe ~optimize m =
    let+ m = until_typecheck ~unsafe m in
    if optimize then Optimize.modul m else m

  let until_link ~unsafe ~optimize ~name link_state m =
    let* m = until_optimize ~unsafe ~optimize m in
    Link.modul link_state ~name m

  let until_interpret ~unsafe ~optimize ~name link_state m =
    let* m, link_state = until_link ~unsafe ~optimize ~name link_state m in
    let+ () = Interpret.Concrete.modul link_state.envs m in
    link_state
end

module Any = struct
  let until_typecheck ~unsafe = function
    | Kind.Wat m -> Text.until_typecheck ~unsafe m
    | Wasm m -> Binary.until_typecheck ~unsafe m
    | Wast _ | Ocaml _ -> assert false

  let until_optimize ~unsafe ~optimize = function
    | Kind.Wat m -> Text.until_optimize ~unsafe ~optimize m
    | Wasm m -> Binary.until_optimize ~unsafe ~optimize m
    | Wast _ | Ocaml _ -> assert false

  let until_link ~unsafe ~optimize ~name link_state = function
    | Kind.Wat m -> Text.until_link ~unsafe ~optimize ~name link_state m
    | Wasm m -> Binary.until_link ~unsafe ~optimize ~name link_state m
    | Wast _ | Ocaml _ -> assert false

  let until_interpret ~unsafe ~optimize ~name link_state = function
    | Kind.Wat m -> Text.until_interpret ~unsafe ~optimize ~name link_state m
    | Wasm m -> Binary.until_interpret ~unsafe ~optimize ~name link_state m
    | Wast _ | Ocaml _ -> assert false
end

module File = struct
  let until_typecheck ~unsafe filename =
    let* m = Parse.guess_from_file filename in
    match m with
    | Kind.Wat m -> Text.until_typecheck ~unsafe m
    | Wasm m -> Binary.until_typecheck ~unsafe m
    | Wast _ | Ocaml _ -> assert false

  let until_optimize ~unsafe ~optimize filename =
    let* m = Parse.guess_from_file filename in
    match m with
    | Kind.Wat m -> Text.until_optimize ~unsafe ~optimize m
    | Wasm m -> Binary.until_optimize ~unsafe ~optimize m
    | Wast _ | Ocaml _ -> assert false

  let until_link ~unsafe ~optimize ~name link_state filename =
    let* m = Parse.guess_from_file filename in
    match m with
    | Kind.Wat m -> Text.until_link ~unsafe ~optimize ~name link_state m
    | Wasm m -> Binary.until_link ~unsafe ~optimize ~name link_state m
    | Wast _ | Ocaml _ -> assert false

  let until_interpret ~unsafe ~optimize ~name link_state filename =
    let* m = Parse.guess_from_file filename in
    match m with
    | Kind.Wat m -> Text.until_interpret ~unsafe ~optimize ~name link_state m
    | Wasm m -> Binary.until_interpret ~unsafe ~optimize ~name link_state m
    | Wast _ | Ocaml _ -> assert false
end
