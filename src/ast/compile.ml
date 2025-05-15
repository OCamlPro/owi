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

  let until_binary_validate ~unsafe ~rac ~srac m =
    let* m = until_binary ~unsafe ~rac ~srac m in
    if unsafe then Ok m
    else
      let+ () = Binary_validate.modul m in
      m

  let until_optimize ~unsafe ~rac ~srac ~optimize m =
    let+ m = until_binary_validate ~unsafe ~rac ~srac m in
    if optimize then Optimize.modul m else m

  let until_link ~unsafe ~rac ~srac ~optimize ~name link_state m =
    let* m = until_optimize ~unsafe ~rac ~srac ~optimize m in
    Link.modul link_state ~name m

  let until_interpret ~unsafe ~timeout ~timeout_instr ~rac ~srac ~optimize ~name
    link_state m =
    let* m, link_state =
      until_link ~unsafe ~rac ~srac ~optimize ~name link_state m
    in
    let+ () =
      Interpret.Concrete.modul ~timeout ~timeout_instr link_state.envs m
    in
    link_state
end

module Binary = struct
  let until_binary_validate ~unsafe m =
    if unsafe then Ok m
    else
      let+ () = Binary_validate.modul m in
      m

  let until_optimize ~unsafe ~optimize m =
    let+ m = until_binary_validate ~unsafe m in
    if optimize then Optimize.modul m else m

  let until_link ~unsafe ~optimize ~name link_state m =
    let* m = until_optimize ~unsafe ~optimize m in
    Link.modul link_state ~name m

  let until_interpret ~unsafe ~timeout ~timeout_instr ~optimize ~name link_state
    m =
    let* m, link_state = until_link ~unsafe ~optimize ~name link_state m in
    let+ () =
      Interpret.Concrete.modul ~timeout ~timeout_instr link_state.envs m
    in
    link_state
end

module Any = struct
  let until_binary_validate ~unsafe ~rac ~srac = function
    | Kind.Wat m -> Text.until_binary_validate ~unsafe ~rac ~srac m
    | Wasm m -> Binary.until_binary_validate ~unsafe m
    | Wast _ | Ocaml _ -> assert false

  let until_optimize ~unsafe ~rac ~srac ~optimize = function
    | Kind.Wat m -> Text.until_optimize ~unsafe ~rac ~srac ~optimize m
    | Wasm m -> Binary.until_optimize ~unsafe ~optimize m
    | Wast _ | Ocaml _ -> assert false

  let until_link ~unsafe ~rac ~srac ~optimize ~name link_state = function
    | Kind.Wat m ->
      Text.until_link ~unsafe ~rac ~srac ~optimize ~name link_state m
    | Wasm m -> Binary.until_link ~unsafe ~optimize ~name link_state m
    | Wast _ | Ocaml _ -> assert false

  let until_interpret ~unsafe ~timeout ~timeout_instr ~rac ~srac ~optimize ~name
    link_state = function
    | Kind.Wat m ->
      Text.until_interpret ~unsafe ~timeout ~timeout_instr ~rac ~srac ~optimize
        ~name link_state m
    | Wasm m ->
      Binary.until_interpret ~unsafe ~timeout ~timeout_instr ~optimize ~name
        link_state m
    | Wast _ | Ocaml _ -> assert false
end

module File = struct
  let until_binary_validate ~unsafe ~rac ~srac filename =
    let* m = Parse.guess_from_file filename in
    match m with
    | Kind.Wat m -> Text.until_binary_validate ~unsafe ~rac ~srac m
    | Wasm m -> Binary.until_binary_validate ~unsafe m
    | Wast _ | Ocaml _ -> assert false

  let until_optimize ~unsafe ~rac ~srac ~optimize filename =
    let* m = Parse.guess_from_file filename in
    match m with
    | Kind.Wat m -> Text.until_optimize ~unsafe ~rac ~srac ~optimize m
    | Wasm m -> Binary.until_optimize ~unsafe ~optimize m
    | Wast _ | Ocaml _ -> assert false

  let until_link ~unsafe ~rac ~srac ~optimize ~name link_state filename =
    let* m = Parse.guess_from_file filename in
    match m with
    | Kind.Wat m ->
      Text.until_link ~unsafe ~rac ~srac ~optimize ~name link_state m
    | Wasm m -> Binary.until_link ~unsafe ~optimize ~name link_state m
    | Wast _ | Ocaml _ -> assert false

  let until_interpret ~unsafe ~timeout ~timeout_instr ~rac ~srac ~optimize ~name
    link_state filename =
    let* m = Parse.guess_from_file filename in
    match m with
    | Kind.Wat m ->
      Text.until_interpret ~unsafe ~timeout ~timeout_instr ~rac ~srac ~optimize
        ~name link_state m
    | Wasm m ->
      Binary.until_interpret ~unsafe ~timeout ~timeout_instr ~optimize ~name
        link_state m
    | Wast _ | Ocaml _ -> assert false
end
