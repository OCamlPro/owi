type token =
  | Open_scope of string
  | Close_scope
  | Symbol of Smtml.Symbol.t

type t = token list

let symbol s tl : t = Symbol s :: tl

let open_scope name tl = Open_scope name :: tl

let close_scope tl = Close_scope :: tl

let empty = []

let of_expressions exprs =
  List.fold_left
    (fun acc cur -> Symbol cur :: acc)
    []
    (Smtml.Expr.Set.get_symbols exprs)

let of_symbol sym = [ Symbol sym ]

let pp_token fmt token =
  match token with
  | Open_scope name -> Fmt.pf fmt "open %s" name
  | Close_scope -> Fmt.pf fmt "close"
  | Symbol s -> Fmt.pf fmt "symbol %a" Smtml.Symbol.pp s

let pp fmt tokens =
  Fmt.list ~sep:(fun fmt () -> Fmt.pf fmt " ; ") pp_token fmt tokens

let only_symbols tokens =
  List.filter_map
    (function Symbol s -> Some s | Open_scope _ | Close_scope -> None)
    tokens

let get_value model sym ty =
  match Smtml.Model.evaluate model sym with
  | Some v -> v
  | None ->
    (* The symbol was created but is not part of the generated model. Thus we can use a dummy value. *)
    (* TODO: allows to hide these symbols or their value through a flag (and make it the default, the flag should actually display them). *)
    Smtml.Value.default_of_type ty

let to_scfg ~no_value model scope_tokens =
  let open Scfg.Types in
  let symbol_to_scfg symbol =
    let open Smtml in
    let name = Symbol.to_string symbol in
    let ty = Symbol.type_of symbol in
    let ty_s = Ty.string_of_type ty in
    let params =
      if no_value then [ name; ty_s ]
      else
        let value = get_value model symbol ty in
        let value = Value.to_string value in
        [ name; ty_s; value ]
    in
    { name = "symbol"; params; children = [] }
  in

  let rec process_scope name tokens =
    let rec collect_items acc tokens =
      match tokens with
      | [] ->
        Log.err (fun m -> m "Reached end without closing scope: %s" name);
        assert false
      | Close_scope :: rest -> (List.rev acc, rest)
      | Open_scope name :: rest ->
        let content, remaining = process_scope name rest in
        let scope =
          match content with
          | sym :: [] ->
            { name = sym.name; params = sym.params @ [ name ]; children = [] }
          | children -> { name = "scope"; params = [ name ]; children }
        in
        collect_items (scope :: acc) remaining
      | Symbol s :: rest -> collect_items (symbol_to_scfg s :: acc) rest
    in
    collect_items [] tokens
  in

  let rec process_tokens tokens acc =
    match tokens with
    | [] -> List.rev acc
    | Open_scope name :: rest ->
      let content, remaining = process_scope name rest in
      let scope =
        match content with
        | sym :: [] ->
          { name = sym.name; params = sym.params @ [ name ]; children = [] }
        | _ as children -> { name = "scope"; params = [ name ]; children }
      in
      process_tokens remaining (scope :: acc)
    | Symbol s :: rest -> process_tokens rest (symbol_to_scfg s :: acc)
    | Close_scope :: _ -> assert false
  in

  let rev_scope_tokens = List.rev scope_tokens in
  let children = process_tokens rev_scope_tokens [] in
  [ { name = "model"; params = []; children } ]

let to_json ~no_value model scope_tokens =
  let open Smtml in
  let module Json = Yojson.Basic in
  let symbol_to_json sym =
    let name = ("symbol", `String (Symbol.to_string sym)) in
    let ty = ("type", `String (Fmt.str "%a" Ty.pp sym.ty)) in
    if no_value then `Assoc [ name; ty ]
    else
      let value = get_value model sym sym.ty in
      let value = ("value", Value.to_json value) in
      `Assoc [ name; ty; value ]
  in

  let rec process_scope scope_name tokens =
    let rec collect_items acc tokens =
      match tokens with
      | [] ->
        Log.err (fun m -> m "Reached end without closing scope: %s" scope_name);
        assert false
      | Open_scope scope_name :: rest ->
        (* Nested scope *)
        let content, remaining = process_scope scope_name rest in
        let json =
          match content with
          | `Assoc pairs :: [] ->
            let obj = `Assoc pairs in
            let scope_json = `Assoc [ ("scope", `String scope_name) ] in
            Json.Util.combine obj scope_json
          | _ ->
            `Assoc [ ("scope", `String scope_name); ("content", `List content) ]
        in
        collect_items (json :: acc) remaining
      | Symbol sym :: rest -> collect_items (symbol_to_json sym :: acc) rest
      | Close_scope :: rest -> (List.rev acc, rest)
    in
    let content, rest = collect_items [] tokens in
    (content, rest)
  in

  let rec process_tokens tokens acc =
    match tokens with
    | [] -> List.rev acc
    | Open_scope scope_name :: rest -> (
      let content, remaining = process_scope scope_name rest in
      match content with
      | `Assoc pairs :: [] ->
        let obj = `Assoc pairs in
        let scope_json = `Assoc [ ("scope", `String scope_name) ] in
        process_tokens remaining (Json.Util.combine obj scope_json :: acc)
      | _ ->
        let json =
          `Assoc [ ("scope", `String scope_name); ("content", `List content) ]
        in
        process_tokens remaining (json :: acc) )
    | Symbol s :: rest -> process_tokens rest (symbol_to_json s :: acc)
    | Close_scope :: _ ->
      Log.err (fun m ->
        m "Called close_scope without a corresponding open_scope" );
      assert false
  in

  let rev_scope_tokens = List.rev scope_tokens in
  Log.debug (fun m -> m "scope tokens: [%a]" pp rev_scope_tokens);
  let result = process_tokens rev_scope_tokens [] in
  `Assoc [ ("model", `List result) ]

let model_of_json json_str =
  let open Yojson.Basic in
  let open Syntax in
  let tbl = Hashtbl.create 16 in

  let* json =
    try Ok (from_string json_str)
    with _ -> Fmt.error_msg "Invalid json string: %s" json_str
  in

  let rec process_json json =
    match json with
    | `Assoc obj :: rest when List.mem_assoc "symbol" obj ->
      let obj = `Assoc obj in
      let ty = Util.member "type" obj |> Util.to_string in
      let* ty = Smtml.Ty.of_string ty in
      let value =
        match Util.member "value" obj with
        | `Bool x -> Bool.to_string x
        | `Float x -> Float.to_string x
        | `Int x -> Int.to_string x
        | `String x -> x
        | _ -> assert false
      in
      let* value = Smtml.Value.of_string ty value in
      let sym_name = Util.member "symbol" obj |> Util.to_string in
      let key = Smtml.Symbol.make ty sym_name in
      Hashtbl.add tbl key value;
      process_json rest
    | `Assoc obj :: rest when List.mem_assoc "scope" obj ->
      let content = Util.member "content" (`Assoc obj) |> Util.to_list in
      let* () = process_json content in
      process_json rest
    | _ :: _ -> assert false
    | [] -> Ok ()
  in
  match json with
  | `Assoc root -> (
    match List.assoc_opt "model" root with
    | Some items ->
      let+ () = process_json (Util.to_list items) in
      tbl
    | None -> Fmt.error_msg "JSON does not contain model field" )
  | _ ->
    Fmt.error_msg "Invalid JSON format for symbol scope:@. %a"
      (Yojson.Basic.pretty_print ~std:true)
      json

let model_of_scfg scfg =
  let open Scfg in
  let open Syntax in
  let tbl = Hashtbl.create 16 in

  let rec process_dirs (dirs : Types.directive list) =
    match dirs with
    | dir :: rest when String.compare dir.name "symbol" = 0 ->
      let* name = Scfg.Query.get_param 0 dir in
      let* ty = Scfg.Query.get_param 1 dir in
      let* ty = Smtml.Ty.of_string ty in
      let* value = Scfg.Query.get_param 2 dir in
      let* value = Smtml.Value.of_string ty value in
      let key = Smtml.Symbol.make ty name in
      Hashtbl.add tbl key value;
      process_dirs rest
    | dir :: rest when String.compare dir.name "scope" = 0 ->
      let* () = process_dirs dir.children in
      process_dirs rest
    | _ :: rest -> process_dirs rest (* not scope or symbol *)
    | [] -> Ok ()
  in

  let* scfg = Scfg.Parse.from_string scfg in
  match Query.get_dir "model" scfg with
  | Some model ->
    let+ () = process_dirs model.children in
    tbl
  | None ->
    Fmt.error_msg "Could not find the directive `model` in the scfg config"
