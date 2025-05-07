type token =
  | Open_scope of string
  | Close_scope
  | Symbol of Smtml.Symbol.t

type t = token list

let symbol s = Symbol s

let open_scope name = Open_scope name

let close_scope = Close_scope

let empty = []

let push token scope = token :: scope

let of_expressions exprs = List.map symbol (Smtml.Expr.Set.get_symbols exprs)

let of_symbol sym = [ Symbol sym ]

let pp fmt token =
  match token with
  | Open_scope name -> Fmt.pf fmt "open %s" name
  | Close_scope -> Fmt.pf fmt "close"
  | Symbol _ -> Fmt.pf fmt "symbol"

let pp_list fmt tokens =
  Fmt.list ~sep:(fun fmt () -> Fmt.pf fmt ";") pp fmt tokens

let only_symbols tokens =
  List.filter_map
    (function Symbol s -> Some s | Open_scope _ | Close_scope -> None)
    tokens

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
  let rec process_scope name tokens =
    let rec collect_items acc tokens =
      match tokens with
      | [] ->
        Logs.err (fun m -> m "Reached end without closing scope: %s" name);
        assert false
      | Close_scope :: rest -> (List.rev acc, rest)
      | Open_scope name :: rest ->
        let content, remaining = process_scope name rest in
        let scope =
          match content with
          | sym :: [] ->
            { name = sym.name; params = name :: sym.params; children = [] }
          | _ as children -> { name = "scope"; params = [ name ]; children }
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
          { name = sym.name; params = name :: sym.params; children = [] }
        | _ as children -> { name = "scope"; params = [ name ]; children }
      in
      process_tokens remaining (scope :: acc)
    | Symbol s :: rest -> process_tokens rest (symbol_to_scfg s :: acc)
    | Close_scope :: _ -> assert false
  in
  let rev_scope_tokens = List.rev scope_tokens in
  Logs.debug (fun m -> m "Current scope tokens: [%a]" pp_list rev_scope_tokens);
  let children = process_tokens rev_scope_tokens [] in
  [ { name = "model"; params = []; children } ]

let to_json ~no_value model scope_tokens =
  let open Smtml in
  let open Yojson.Basic in
  let symbol_to_json sym =
    let name = ("symbol", `String (Symbol.to_string sym)) in
    let ty = ("type", `String (Fmt.str "%a" Ty.pp sym.ty)) in
    if no_value then `Assoc [ name; ty ]
    else
      let value = ("value", Value.to_json @@ get_value model sym) in
      `Assoc [ name; ty; value ]
  in

  let rec process_scope scope_name tokens =
    let rec collect_items acc tokens =
      match tokens with
      | [] ->
        Logs.err (fun m -> m "Reached end without closing scope: %s" scope_name);
        assert false
      | Open_scope scope_name :: rest ->
        (* Nested scope *)
        let content, remaining = process_scope scope_name rest in
        let json =
          match content with
          | `Assoc pairs :: [] ->
            let obj = `Assoc pairs in
            let scope_json = `Assoc [ ("scope", `String scope_name) ] in
            Util.combine obj scope_json
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
        process_tokens remaining (Util.combine obj scope_json :: acc)
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
  Logs.debug (fun m -> m "Current scope tokens: [%a]" pp_list rev_scope_tokens);
  let result = process_tokens rev_scope_tokens [] in
  `Assoc [ ("model", `List result) ]
