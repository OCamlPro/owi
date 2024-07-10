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

  let until_link ~unsafe link_state ~optimize ~name m =
    let* m = until_optimize ~unsafe ~optimize m in
    Link.modul link_state ~name m

  let until_interpret link_state ~unsafe ~optimize ~name m =
    let* m, link_state = until_link link_state ~unsafe ~optimize ~name m in
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

  let until_link ~unsafe link_state ~optimize ~name m =
    let* m = until_optimize ~unsafe ~optimize m in
    Link.modul link_state ~name m

  let until_interpret link_state ~unsafe ~optimize ~name m =
    let* m =
      if unsafe then Ok m
      else
        let+ () = Typecheck.modul m in
        m
    in
    let* m = if optimize then Ok (Optimize.modul m) else Ok m in
    let* m, link_state = Link.modul link_state ~name m in
    let+ () = Interpret.Concrete.modul link_state.envs m in
    link_state
end

module ApiV2 = struct
  type 'a t =
       'a Link.module_to_run list * 'a Link.state
    -> ('a Link.module_to_run list * 'a Link.state) Result.t

  let start_compilation = Ok ([], Link.empty_state)

  let extern_module ~func_typ ~name ~module_impl (modules_to_run, state) =
    let state = Link.extern_module' state ~name ~func_typ module_impl in
    Ok (modules_to_run, state)

  let file ~filename ~unsafe ~optimize ~add_main_as_start (modules_to_run, state)
      =
    let* m = Parse.guess_from_file filename in
    let* m =
      match m with
      | Either.Left (Either.Left text_module) ->
        Text.until_binary ~unsafe text_module
      | Either.Left (Either.Right _text_scrpt) ->
        Error (`Msg "Wasm script is not supported by this API.")
      | Either.Right binary_module -> Ok binary_module
    in
    let* m =
      if add_main_as_start then Cmd_utils.add_main_as_start m else Ok m
    in
    let name = filename |> Fpath.rem_ext |> Fpath.filename |> Option.some in
    let+ m_to_run, state = Binary.until_link ~unsafe ~name ~optimize state m in
    (m_to_run :: modules_to_run, state)

  let files ~filenames ~unsafe ~optimize ~add_main_as_start init =
    List.fold_left
      (fun acc filename ->
        Result.bind acc (file ~filename ~unsafe ~optimize ~add_main_as_start) )
      (Ok init) filenames

  let finish (modules_to_run, state) = Ok (List.rev modules_to_run, state)

  let compile l =
    let* x = List.fold_left Result.bind start_compilation l in
    finish x
end
