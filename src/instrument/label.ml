(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

module Coverage_criteria = struct
  type t =
    | Function_coverage
    | Statement_coverage
    | Decision_coverage

  let of_string = function
    | "fc" -> Ok Function_coverage
    | "sc" -> Ok Statement_coverage
    | "dc" -> Ok Decision_coverage
    | s -> Fmt.error_msg "unknown coverage criteria %s" s

  let pp fmt criteria =
    Fmt.string fmt
    @@
    match criteria with
    | Function_coverage -> "fc"
    | Statement_coverage -> "sc"
    | Decision_coverage -> "dc"
end

let annotate_fc m cov_label_set_idx =
  let count = ref ~-1 in
  let func = m.Binary.Module.func in
  let func =
    Array.map
      (function
        | Origin.Imported _ as v ->
          (* TODO: should we write a wrapper for the imported function so that it gets counted in FC? *)
          v
        | Origin.Local f ->
          let body =
            Annotated.map
              (fun expr ->
                incr count;
                Annotated.dummies
                  [ Binary.I32_const (Int32.of_int !count)
                  ; Call cov_label_set_idx
                  ]
                @ expr )
              f.Binary.Func.body
          in
          let f = { f with body } in
          Origin.Local f )
      func
  in
  ({ m with func }, !count)

let annotate_sc m cov_label_set_idx =
  let count = ref ~-1 in
  let func = m.Binary.Module.func in
  let func =
    Array.map
      (function
        | Origin.Imported _ as v ->
          (* TODO: should we write a wrapper for the imported function so that it gets counted in FC? *)
          v
        | Origin.Local f ->
          let rec handle_instr instr =
            incr count;
            let prefix =
              Annotated.dummies
                [ Binary.I32_const (Int32.of_int !count)
                ; Call cov_label_set_idx
                ]
            in
            let instr =
              Annotated.map
                (function
                  | Binary.Block (id, typ, body) ->
                    let body = handle_expr body in
                    Binary.Block (id, typ, body)
                  | Binary.Loop (id, typ, body) ->
                    let body = handle_expr body in
                    Binary.Loop (id, typ, body)
                  | Binary.If_else (id, typ, true_branch, false_branch) ->
                    let true_branch = handle_expr true_branch in
                    let false_branch = handle_expr false_branch in
                    Binary.If_else (id, typ, true_branch, false_branch)
                  | instr ->
                    (* TODO: make this match non fragile? *)
                    instr )
                instr
            in
            prefix @ [ instr ]
          and handle_expr expr =
            Annotated.map
              (fun expr -> List.map handle_instr expr |> List.flatten)
              expr
          in
          let body = handle_expr f.Binary.Func.body in
          let f = { f with body } in
          Origin.Local f )
      func
  in
  ({ m with func }, !count)

let annotate_dc m cov_label_set_idx =
  let count = ref ~-1 in
  let func = m.Binary.Module.func in
  let func =
    Array.map
      (function
        | Origin.Imported _ as v -> v
        | Origin.Local f ->
          let rec handle_instr instr =
            let instr =
              Annotated.map
                (function
                  | Binary.Br_if idx ->
                    incr count;
                    let count_true = !count in
                    incr count;
                    let count_false = !count in
                    Binary.If_else
                      ( None
                      , None
                      , Annotated.dummy_deep
                          [ Binary.I32_const (Int32.of_int count_true)
                          ; Call cov_label_set_idx
                          ; Br (idx + 1)
                          ]
                      , Annotated.dummy_deep
                          [ Binary.I32_const (Int32.of_int count_false)
                          ; Call cov_label_set_idx
                          ] )
                  | Binary.If_else (id, typ, true_branch, false_branch) ->
                    incr count;
                    let count_true = !count in
                    let true_branch =
                      Annotated.map
                        (fun expr ->
                          Annotated.dummies
                            [ Binary.I32_const (Int32.of_int count_true)
                            ; Call cov_label_set_idx
                            ]
                          @ expr )
                        true_branch
                    in

                    incr count;
                    let count_false = !count in
                    let false_branch =
                      Annotated.map
                        (fun expr ->
                          Annotated.dummies
                            [ Binary.I32_const (Int32.of_int count_false)
                            ; Call cov_label_set_idx
                            ]
                          @ expr )
                        false_branch
                    in

                    let true_branch = handle_expr true_branch in
                    let false_branch = handle_expr false_branch in
                    Binary.If_else (id, typ, true_branch, false_branch)
                  | Binary.Block (id, typ, body) ->
                    let body = handle_expr body in
                    Binary.Block (id, typ, body)
                  | Binary.Loop (id, typ, body) ->
                    let body = handle_expr body in
                    Binary.Loop (id, typ, body)
                  | instr ->
                    (* TODO: make this match non fragile? *)
                    instr )
                instr
            in
            [ instr ]
          and handle_expr expr =
            Annotated.map
              (fun expr -> List.map handle_instr expr |> List.flatten)
              expr
          in
          let body = handle_expr f.Binary.Func.body in
          let f = { f with body } in
          Origin.Local f )
      func
  in
  ({ m with func }, !count)

let annotate criteria m =
  let m =
    let typ =
      Binary.Bt_raw
        (None, ([ (None, Text.Num_type I32); (None, Num_type I32) ], []))
    in
    Binary.Module.add_import_if_not_present ~modul_name:"owi"
      ~func_name:"cov_label_set" ~typ m
  in
  let cov_label_set_idx =
    match
      Binary.Module.find_imported_func_index ~modul_name:"owi"
        ~func_name:"cov_label_set" m
    with
    | None -> assert false
    | Some idx -> idx
  in
  let m, label_count =
    match criteria with
    | Coverage_criteria.Function_coverage -> annotate_fc m cov_label_set_idx
    | Statement_coverage -> annotate_sc m cov_label_set_idx
    | Decision_coverage -> annotate_dc m cov_label_set_idx
  in
  Log.app (fun m -> m "generated %d labels!" (label_count + 1));
  m
