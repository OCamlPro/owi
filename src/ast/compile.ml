(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax

module Text = struct
  let until_text_validate ~unsafe m =
    if unsafe then Ok m else Text_validate.modul m

  let until_group ~unsafe m =
    let* m = until_text_validate ~unsafe m in
    Grouped.of_symbolic m

  let until_assign ~unsafe m =
    let* m = until_group ~unsafe m in
    Assigned.of_grouped m

  let until_binary ~unsafe m =
    let* m = until_assign ~unsafe m in
    Rewrite.modul m

<<<<<<< HEAD
  let until_binary_validate ~unsafe m =
=======
  let until_typecheck ~unsafe ~rac m =
>>>>>>> 66d816d7 (framework for rac code generation)
    let* m = until_binary ~unsafe m in
    let* m = Code_generator.generate rac m in
    if unsafe then Ok m
    else
      let+ () = Binary_validate.modul m in
      m

<<<<<<< HEAD
  let until_optimize ~unsafe ~optimize m =
    let+ m = until_binary_validate ~unsafe m in
=======
  let until_optimize ~unsafe ~rac ~optimize m =
    let+ m = until_typecheck ~unsafe ~rac m in
>>>>>>> 66d816d7 (framework for rac code generation)
    if optimize then Optimize.modul m else m

  let until_link ~unsafe ~rac ~optimize ~name link_state m =
    let* m = until_optimize ~unsafe ~rac ~optimize m in
    Link.modul link_state ~name m

  let until_interpret ~unsafe ~rac ~optimize ~name link_state m =
    let* m, link_state = until_link ~unsafe ~rac ~optimize ~name link_state m in
    let+ () = Interpret.Concrete.modul link_state.envs m in
    link_state
end

module Binary = struct
<<<<<<< HEAD
  let until_binary_validate ~unsafe m =
=======
  let until_typecheck ~unsafe ~rac m =
    let* m = Code_generator.generate rac m in
>>>>>>> 66d816d7 (framework for rac code generation)
    if unsafe then Ok m
    else
      let+ () = Binary_validate.modul m in
      m

<<<<<<< HEAD
  let until_optimize ~unsafe ~optimize m =
    let+ m = until_binary_validate ~unsafe m in
=======
  let until_optimize ~unsafe ~rac ~optimize m =
    let+ m = until_typecheck ~unsafe ~rac m in
>>>>>>> 66d816d7 (framework for rac code generation)
    if optimize then Optimize.modul m else m

  let until_link ~unsafe ~rac ~optimize ~name link_state m =
    let* m = until_optimize ~unsafe ~rac ~optimize m in
    Link.modul link_state ~name m

  let until_interpret ~unsafe ~rac ~optimize ~name link_state m =
    let* m, link_state = until_link ~unsafe ~rac ~optimize ~name link_state m in
    let+ () = Interpret.Concrete.modul link_state.envs m in
    link_state
end

module Any = struct
<<<<<<< HEAD
  let until_binary_validate ~unsafe = function
    | Kind.Wat m -> Text.until_binary_validate ~unsafe m
    | Wasm m -> Binary.until_binary_validate ~unsafe m
=======
  let until_typecheck ~unsafe ~rac = function
    | Kind.Wat m -> Text.until_typecheck ~unsafe ~rac m
    | Wasm m -> Binary.until_typecheck ~unsafe ~rac m
>>>>>>> 66d816d7 (framework for rac code generation)
    | Wast _ | Ocaml _ -> assert false

  let until_optimize ~unsafe ~rac ~optimize = function
    | Kind.Wat m -> Text.until_optimize ~unsafe ~rac ~optimize m
    | Wasm m -> Binary.until_optimize ~unsafe ~rac ~optimize m
    | Wast _ | Ocaml _ -> assert false

  let until_link ~unsafe ~rac ~optimize ~name link_state = function
    | Kind.Wat m -> Text.until_link ~unsafe ~rac ~optimize ~name link_state m
    | Wasm m -> Binary.until_link ~unsafe ~rac ~optimize ~name link_state m
    | Wast _ | Ocaml _ -> assert false

  let until_interpret ~unsafe ~rac ~optimize ~name link_state = function
    | Kind.Wat m ->
      Text.until_interpret ~unsafe ~rac ~optimize ~name link_state m
    | Wasm m -> Binary.until_interpret ~unsafe ~rac ~optimize ~name link_state m
    | Wast _ | Ocaml _ -> assert false
end

module File = struct
<<<<<<< HEAD
  let until_binary_validate ~unsafe filename =
    let* m = Parse.guess_from_file filename in
    match m with
    | Kind.Wat m -> Text.until_binary_validate ~unsafe m
    | Wasm m -> Binary.until_binary_validate ~unsafe m
=======
  let until_typecheck ~unsafe ~rac filename =
    let* m = Parse.guess_from_file filename in
    match m with
    | Kind.Wat m -> Text.until_typecheck ~unsafe ~rac m
    | Wasm m -> Binary.until_typecheck ~unsafe ~rac m
>>>>>>> 66d816d7 (framework for rac code generation)
    | Wast _ | Ocaml _ -> assert false

  let until_optimize ~unsafe ~rac ~optimize filename =
    let* m = Parse.guess_from_file filename in
    match m with
    | Kind.Wat m -> Text.until_optimize ~unsafe ~rac ~optimize m
    | Wasm m -> Binary.until_optimize ~unsafe ~rac ~optimize m
    | Wast _ | Ocaml _ -> assert false

  let until_link ~unsafe ~rac ~optimize ~name link_state filename =
    let* m = Parse.guess_from_file filename in
    match m with
    | Kind.Wat m -> Text.until_link ~unsafe ~rac ~optimize ~name link_state m
    | Wasm m -> Binary.until_link ~unsafe ~rac ~optimize ~name link_state m
    | Wast _ | Ocaml _ -> assert false

  let until_interpret ~unsafe ~rac ~optimize ~name link_state filename =
    let* m = Parse.guess_from_file filename in
    match m with
    | Kind.Wat m ->
      Text.until_interpret ~unsafe ~rac ~optimize ~name link_state m
    | Wasm m -> Binary.until_interpret ~unsafe ~rac ~optimize ~name link_state m
    | Wast _ | Ocaml _ -> assert false
end
