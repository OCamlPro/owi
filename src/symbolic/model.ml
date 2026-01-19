(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax

(* model stuff *)
type output_format =
  | Scfg
  | Json

let pp format with_breadcrumbs no_value fmt
  (model, { Thread.symbol_scopes; labels; breadcrumbs; _ }) =
  match format with
  | Json ->
    let json = Symbol_scope.to_json ~no_value model symbol_scopes in
    let labels_json =
      List.map
        (fun (id, name) -> `Assoc [ ("id", `Int id); ("name", `String name) ])
        labels
    in
    let json =
      match json with
      | `Assoc fields -> `Assoc (("labels", `List labels_json) :: fields)
      | _ -> json
    in
    let json =
      if with_breadcrumbs then
        let crumbs =
          List.rev_map (fun crumb -> `Int crumb) (List.rev breadcrumbs)
        in
        match json with
        | `Assoc fields -> `Assoc (fields @ [ ("breadcrumbs", `List crumbs) ])
        | _ -> json
      else json
    in
    Yojson.Basic.pretty_print fmt json
  | Scfg ->
    let scfg = Symbol_scope.to_scfg ~no_value model symbol_scopes in
    let model = Scfg.Query.get_dir_exn "model" scfg in
    let lbls =
      { Scfg.Types.name = "labels"
      ; params = []
      ; children =
          List.map
            (fun (id, lbl_name) ->
              { Scfg.Types.name = "label"
              ; params = [ string_of_int id; lbl_name ]
              ; children = []
              } )
            labels
      }
    in
    let bcrumbs =
      if with_breadcrumbs then
        [ { Scfg.Types.name = "breadcrumbs"
          ; params = List.map string_of_int (List.rev breadcrumbs)
          ; children = []
          }
        ]
      else []
    in
    let ret =
      model
      :: (if List.length lbls.children > 0 then lbls :: bcrumbs else bcrumbs)
    in
    Scfg.Pp.config fmt ret

let print ~format ~out_file ~id ~no_value ~no_stop_at_failure
  ~no_assert_failure_expression_printing ~with_breadcrumbs
  ({ Bug.model; thread; _ } as bug) =
  begin match bug.Bug.kind with
  | `Trap trap -> Log.err (fun m -> m "Trap: %s" (Result.err_to_string trap))
  | `Assertion assertion ->
    if no_assert_failure_expression_printing then
      Log.err (fun m -> m "Assert failure")
    else Log.err (fun m -> m "Assert failure: %a" Symbolic_boolean.pp assertion)
  end;

  match out_file with
  | None -> begin
    Log.app (fun m ->
      let fmt = m (if no_stop_at_failure then "%a@." else "%a") in
      fmt (pp format with_breadcrumbs no_value) (model, thread) );
    Ok ()
  end
  | Some path ->
    let ext = match format with Json -> "json" | Scfg -> "scfg" in
    let contains_ext =
      Fpath.has_ext ".json" path || Fpath.has_ext ".scfg" path
    in
    let* path =
      let* () =
        if contains_ext && (not @@ Fpath.has_ext ext path) then
          Fmt.error_msg
            "Given model file extension is not compatible with the current \
             model format"
        else Ok ()
      in
      let path = Fpath.to_string @@ Fpath.rem_ext path in
      let base =
        Fpath.v (if no_stop_at_failure then Fmt.str "%s_%d" path id else path)
      in
      Ok (Fpath.add_ext ext base)
    in
    Bos.OS.File.writef path "%a"
      (pp format with_breadcrumbs no_value)
      (model, thread)
