type scope_token =
  | Open_scope of string
  | Close_scope
  | Symbol of Smtml.Symbol.t

let pp fmt token =
  match token with
  | Open_scope name -> Fmt.pf fmt "open %s" name
  | Close_scope -> Fmt.pf fmt "close"
  | Symbol _ -> Fmt.pf fmt "symbol"

let pp_list fmt tokens =
  Fmt.list ~sep:(fun fmt () -> Fmt.pf fmt " ") pp fmt tokens

let symbol s = Symbol s

let only_symbols tokens =
  List.filter_map (function Symbol s -> Some s | _ -> None) tokens

let get_value model sym =
  match Smtml.Model.evaluate model sym with
  | Some value -> value
  | None -> assert false

let to_scfg ~no_value model scope_tokens =
  let open Scfg.Types in
  let open Smtml in
  let symbol_to_scfg symbol =
    let p0 = Symbol.to_string symbol in
    let p1 = Symbol.type_of symbol |> Ty.string_of_type in
    let params =
      if no_value then [ p0; p1 ]
      else
        let value = get_value model symbol in
        let p2 = Value.to_string value in
        [ p0; p1; p2 ]
    in
    { name = "symbol"; params; children = [] }
  in
  let rec process_scope tokens =
    let rec collect_items acc tokens =
      match tokens with
      | [] -> assert false
      | Close_scope :: rest ->
        let result = List.rev acc in
        (result, rest)
      | Open_scope name :: rest ->
        let children, remaining = process_scope rest in
        let scope = { name = "scope"; params = [ name ]; children } in
        collect_items (scope :: acc) remaining
      | Symbol s :: rest -> collect_items (symbol_to_scfg s :: acc) rest
    in
    collect_items [] tokens
  in
  let rec process_tokens tokens acc =
    match tokens with
    | [] -> List.rev acc
    | Open_scope name :: rest ->
      let children, remaining = process_scope rest in
      let scope = { name = "scope"; params = [ name ]; children } in
      process_tokens remaining (scope :: acc)
    | Symbol s :: rest -> process_tokens rest (symbol_to_scfg s :: acc)
    | Close_scope :: _ -> assert false
  in
  Log.debug (fun m -> m "Current scope tokens: %a" pp_list scope_tokens);
  let children = process_tokens (List.rev scope_tokens) [] in
  [ { name = "model"; params = []; children } ]

let to_json model scope_tokens =
  let open Smtml in
  let rec process_scope tokens =
    let rec collect_items acc tokens =
      match tokens with
      | [] -> assert false
      | Close_scope :: rest -> (List.rev acc, rest)
      | Open_scope name :: rest ->
        (* Nested scope *)
        let content, remaining = process_scope rest in
        let scope_json = `Assoc [ (name, content) ] in
        collect_items (scope_json :: acc) remaining
      | Symbol sym :: rest -> (
        match Symbol.to_json sym with
        | `Assoc [ (name, props) ] ->
          let value = get_value model sym in
          let value = `Assoc [ ("value", Value.to_json value) ] in
          (`Assoc [ (name, Yojson.Basic.Util.combine props value) ] :: acc, rest)
        | _ ->
          Fmt.failwith "Model: Symbol.to_json returned something impossible" )
    in
    let content, rest = collect_items [] tokens in
    (`List content, rest)
  in

  let rec process_tokens tokens acc =
    match tokens with
    | [] -> `List (List.rev acc)
    | Open_scope name :: rest ->
      let content, remaining = process_scope rest in
      let scope_json = `Assoc [ (name, content) ] in
      process_tokens remaining (scope_json :: acc)
    | Symbol s :: rest -> process_tokens rest (Symbol.to_json s :: acc)
    | Close_scope :: _ ->
      (* We should not have a close_scope before an open_scope *)
      assert false
  in
  let result = process_tokens scope_tokens [] in
  `Assoc [ ("model", result) ]
