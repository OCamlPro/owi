(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax

module Text = struct
  let until_text_validate ~unsafe m =
    if unsafe then Ok m else Text_validate.modul m

  let until_group ~unsafe m =
    let+ m = until_text_validate ~unsafe m in
    Grouped.of_text m

  let until_assign ~unsafe m =
    let* m = until_group ~unsafe m in
    let+ assigned = Assigned.of_grouped m in
    (m, assigned)

  let until_binary ~unsafe m =
    let* m, assigned = until_assign ~unsafe m in
    Rewrite.modul m assigned

  let until_validate ~unsafe m =
    let* m = until_text_validate ~unsafe m in
    let* m = until_binary ~unsafe m in
    if unsafe then Ok m
    else
      let+ () = Binary_validate.modul m in
      m

  let until_concrete_link ~unsafe ~name env m =
    let* m = until_validate ~unsafe m in
    Concrete_env.link_binary_module env ~name m

  let until_symbolic_link ~unsafe ~name env m =
    let* m = until_validate ~unsafe m in
    Symbolic_env.link_binary_module env ~name m

  let until_abstract_link ~unsafe ~name runtime m =
    let* modul = until_validate ~unsafe m in
    let* runtime = Abstract_runtime.link_binary_module ~runtime ~name ~modul in
    let+ modul = Abstract_runtime.get_last_module ~runtime in
    (modul, runtime)
end

module Binary = struct
  let until_validate ~unsafe m =
    if unsafe then Ok m
    else
      let+ () = Binary_validate.modul m in
      m

  let until_concrete_link ~unsafe ~name env m =
    let* m = until_validate ~unsafe m in
    Concrete_env.link_binary_module env ~name m

  let until_symbolic_link ~unsafe ~name env m =
    let* m = until_validate ~unsafe m in
    Symbolic_env.link_binary_module env ~name m

  let until_abstract_link ~unsafe ~name runtime m =
    let* modul = until_validate ~unsafe m in
    let* runtime = Abstract_runtime.link_binary_module ~runtime ~name ~modul in
    let+ modul = Abstract_runtime.get_last_module ~runtime in
    (modul, runtime)
end

module Any = struct
  let until_validate ~unsafe = function
    | Kind.Wat m -> Text.until_validate ~unsafe m
    | Wasm m -> Binary.until_validate ~unsafe m
    | Wast _ -> Fmt.error_msg "can not validate a .wast file"
    | Extern _ -> Fmt.error_msg "can not validate an OCaml module"

  let until_concrete_link ~unsafe ~name env = function
    | Kind.Wat m -> Text.until_concrete_link ~unsafe ~name env m
    | Wasm m -> Binary.until_concrete_link ~unsafe ~name env m
    | Extern _m ->
      (* TODO: Link.Extern.modul m *)
      Fmt.error_msg "can not link an OCaml module"
    | Wast _ -> Fmt.error_msg "can not link a .wast file"

  let until_symbolic_link ~unsafe ~name env = function
    | Kind.Wat m -> Text.until_symbolic_link ~unsafe ~name env m
    | Wasm m -> Binary.until_symbolic_link ~unsafe ~name env m
    | Extern _m ->
      (* TODO: Link.Extern.modul m *)
      Fmt.error_msg "can not link an OCaml module"
    | Wast _ -> Fmt.error_msg "can not link a .wast file"

  let until_abstract_link ~unsafe ~name env = function
    | Kind.Wat m -> Text.until_abstract_link ~unsafe ~name env m
    | Wasm m -> Binary.until_abstract_link ~unsafe ~name env m
    | Extern _m ->
      (* TODO: Link.Extern.modul m *)
      Fmt.error_msg "can not link an OCaml module"
    | Wast _ -> Fmt.error_msg "can not link a .wast file"
end

module File = struct
  let until_binary ~unsafe filename =
    let* m = Parse.guess_from_file filename in
    match m with
    | Kind.Wat m -> Text.until_binary ~unsafe m
    | Wasm m -> Ok m
    | Wast _ | Extern _ -> assert false

  let until_validate ~unsafe filename =
    let* m = Parse.guess_from_file filename in
    Log.bench_fn "validation time" @@ fun () ->
    match m with
    | Kind.Wat m -> Text.until_validate ~unsafe m
    | Wasm m -> Binary.until_validate ~unsafe m
    | Wast _ | Extern _ -> assert false

  let until_concrete_link ~unsafe ~name env filename =
    let* m = Parse.guess_from_file filename in
    match m with
    | Kind.Wat m -> Text.until_concrete_link ~unsafe ~name env m
    | Wasm m -> Binary.until_concrete_link ~unsafe ~name env m
    | Wast _ | Extern _ -> assert false

  let until_symbolic_link ~unsafe ~name env filename =
    let* m = Parse.guess_from_file filename in
    match m with
    | Kind.Wat m -> Text.until_symbolic_link ~unsafe ~name env m
    | Wasm m -> Binary.until_symbolic_link ~unsafe ~name env m
    | Wast _ | Extern _ -> assert false

  let until_abstract_link ~unsafe ~name env filename =
    let* m = Parse.guess_from_file filename in
    match m with
    | Kind.Wat m -> Text.until_abstract_link ~unsafe ~name env m
    | Wasm m -> Binary.until_abstract_link ~unsafe ~name env m
    | Wast _ | Extern _ -> assert false
end
