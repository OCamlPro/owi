(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
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
    let* m = until_binary ~unsafe m in
    if unsafe then Ok m
    else
      let+ () = Binary_validate.modul m in
      m

  let until_link ~unsafe ~name link_state m =
    let* m = until_validate ~unsafe m in
    Link.Binary.modul link_state ~name m
end

module Binary = struct
  let until_validate ~unsafe m =
    if unsafe then Ok m
    else
      let+ () = Binary_validate.modul m in
      m

  let until_link ~unsafe ~name link_state m =
    let* m = until_validate ~unsafe m in
    Link.Binary.modul link_state ~name m
end

module Any = struct
  let until_validate ~unsafe = function
    | Kind.Wat m -> Text.until_validate ~unsafe m
    | Wasm m -> Binary.until_validate ~unsafe m
    | Wast _ -> Fmt.error_msg "can not validate a .wast file"
    | Extern _ -> Fmt.error_msg "can not validate an OCaml module"

  let until_link ~unsafe ~name link_state = function
    | Kind.Wat m -> Text.until_link ~unsafe ~name link_state m
    | Wasm m -> Binary.until_link ~unsafe ~name link_state m
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

  let until_link ~unsafe ~name link_state filename =
    let* m = Parse.guess_from_file filename in
    match m with
    | Kind.Wat m -> Text.until_link ~unsafe ~name link_state m
    | Wasm m -> Binary.until_link ~unsafe ~name link_state m
    | Wast _ | Extern _ -> assert false
end
