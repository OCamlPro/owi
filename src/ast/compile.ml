(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax

module Text = struct
  let until_text_validate ~unsafe m =
    if unsafe then Ok m else Text_validate.modul m

  let until_group ~unsafe m =
    let* m = until_text_validate ~unsafe m in
    Grouped.of_text m

  let until_assign ~unsafe m =
    let* m = until_group ~unsafe m in
    Assigned.of_grouped m

  let until_binary ~unsafe ~rac ~srac m =
    let* m = until_assign ~unsafe m in
    let* m = Rewrite.modul m in
    if srac then Code_generator.generate true m
    else if rac then Code_generator.generate false m
    else Ok m

  let until_validate ~unsafe ~rac ~srac m =
    let* m = until_binary ~unsafe ~rac ~srac m in
    if unsafe then Ok m
    else
      let+ () = Binary_validate.modul m in
      m

  let until_link ~unsafe ~rac ~srac ~name link_state m =
    let* m = until_validate ~unsafe ~rac ~srac m in
    Link.modul link_state ~name m
end

module Binary = struct
  let until_validate ~unsafe m =
    if unsafe then Ok m
    else
      let+ () = Binary_validate.modul m in
      m

  let until_link ~unsafe ~name link_state m =
    let* m = until_validate ~unsafe m in
    Link.modul link_state ~name m
end

module Any = struct
  let until_validate ~unsafe ~rac ~srac = function
    | Kind.Wat m -> Text.until_validate ~unsafe ~rac ~srac m
    | Wasm m -> Binary.until_validate ~unsafe m
    | Wast _ -> Fmt.error_msg "can not validate a .wast file"
    | Ocaml _ -> Fmt.error_msg "can not validate an OCaml module"

  let until_link ~unsafe ~rac ~srac ~name link_state = function
    | Kind.Wat m -> Text.until_link ~unsafe ~rac ~srac ~name link_state m
    | Wasm m -> Binary.until_link ~unsafe ~name link_state m
    | Ocaml _m ->
      (* TODO: we may be able to handle linking here but return an empty runnable module ? *)
      Fmt.error_msg "can not link an OCaml module"
    | Wast _ -> Fmt.error_msg "can not link a .wast file"
end

module File = struct
  let until_validate ~unsafe ~rac ~srac filename =
    let* m = Parse.guess_from_file filename in
    match m with
    | Kind.Wat m -> Text.until_validate ~unsafe ~rac ~srac m
    | Wasm m -> Binary.until_validate ~unsafe m
    | Wast _ | Ocaml _ -> assert false

  let until_link ~unsafe ~rac ~srac ~name link_state filename =
    let* m = Parse.guess_from_file filename in
    match m with
    | Kind.Wat m -> Text.until_link ~unsafe ~rac ~srac ~name link_state m
    | Wasm m -> Binary.until_link ~unsafe ~name link_state m
    | Wast _ | Ocaml _ -> assert false
end
