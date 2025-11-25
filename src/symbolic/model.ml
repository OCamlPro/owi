(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax

(* model stuff *)
type output_format =
  | Scfg
  | Json

let pp format with_breadcrumbs no_value fmt
  (model, labels, breadcrumbs, scoped_values, stats) =
  match format with
  | Json ->
    let json = Symbol_scope.to_json ~no_value model scoped_values in
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
    let json =
      match json with
      | `Assoc fields when Log.is_bench_enabled () ->
        let solver_stats_json : Yojson.Basic.t =
          `Assoc
            (Solver.fold_stats
               (fun id v acc ->
                 match v with
                 | `Int i -> (id, `Int i) :: acc
                 | `Float f -> (id, `Float f) :: acc )
               stats [] )
        in
        `Assoc (("solver_stats", solver_stats_json) :: fields)
      | _ -> json
    in
    Yojson.Basic.pretty_print fmt json
  | Scfg ->
    let scfg = Symbol_scope.to_scfg ~no_value model scoped_values in
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
    let bcrumbs =
      if Log.is_bench_enabled () then
        let stats =
          { Scfg.Types.name = "solver_stats"
          ; params = []
          ; children =
              Solver.fold_stats
                (fun id v acc ->
                  let params =
                    match v with
                    | `Int i -> [ id; string_of_int i ]
                    | `Float f -> [ id; string_of_float f ]
                  in
                  { Scfg.Types.name = "stat"; params; children = [] } :: acc )
                stats []
          }
        in
        stats :: bcrumbs
      else bcrumbs
    in
    let ret =
      model
      :: (if List.length lbls.children > 0 then lbls :: bcrumbs else bcrumbs)
    in
    Scfg.Pp.config fmt ret

let print ~format ~out_file ~id ~no_value ~no_stop_at_failure
  ~no_assert_failure_expression_printing ~with_breadcrumbs bug =
  let to_file path model labels breadcrumbs symbol_scopes stats =
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
      (model, labels, breadcrumbs, symbol_scopes, stats)
  in
  let output model labels breadcrumbs symbol_scopes stats =
    match out_file with
    | Some path -> to_file path model labels breadcrumbs symbol_scopes stats
    | None -> begin
      Log.app (fun m ->
        let fmt = m (if no_stop_at_failure then "%a@." else "%a") in
        fmt
          (pp format with_breadcrumbs no_value)
          (model, labels, breadcrumbs, symbol_scopes, stats) );
      Ok ()
    end
  in
  match bug with
  | `ETrap (tr, model, labels, breadcrumbs, symbol_scopes, stats) ->
    Log.err (fun m -> m "Trap: %s" (Result.err_to_string tr));
    output model labels breadcrumbs symbol_scopes stats
  | `EAssert (assertion, model, labels, breadcrumbs, symbol_scopes, stats) ->
    if no_assert_failure_expression_printing then
      Log.err (fun m -> m "Assert failure")
    else Log.err (fun m -> m "Assert failure: %a" Smtml.Expr.pp assertion);
    output model labels breadcrumbs symbol_scopes stats
