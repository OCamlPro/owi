(** Module providing functions to parse a wasm script from various kind of
    inputs. *)

let menhir_rule_from_lexbuf rule =
  let parser = MenhirLib.Convert.Simplified.traditional2revised rule in
  let print_err buf msg =
    let pos = fst @@ Sedlexing.lexing_positions buf in
    let file_line =
      let cpos = pos.pos_cnum - pos.pos_bol in
      Printf.sprintf "File \"%s\", line %i, character %i:" pos.pos_fname
        pos.pos_lnum cpos
    in
    Log.debug "%s@\n%s@\n" file_line msg
  in
  fun buf ->
    Log.debug "parsing      ...@\n";
    let provider () =
      let tok = Lexer.token buf in
      let start, stop = Sedlexing.lexing_positions buf in
      (tok, start, stop)
    in
    try Ok (parser provider) with
    | Types.Parse_fail msg
    | Lexer.Illegal_escape msg
    | Lexer.Unknown_operator msg ->
      print_err buf msg;
      Error msg
    | Lexer.Unexpected_character msg ->
      print_err buf msg;
      Error "unknown operator"
    | Menhir_parser.Error ->
      let msg = "unexpected token" in
      print_err buf msg;
      Error msg

(** Parse a script from a lexing buffer. *)
let script_from_lexbuf = menhir_rule_from_lexbuf Menhir_parser.script

(** Parse a script from a string. *)
let script_from_string s = script_from_lexbuf (Sedlexing.Utf8.from_string s)

(** Parse a script from a channel. *)
let script_from_channel c = script_from_lexbuf (Sedlexing.Utf8.from_channel c)

(** Parse a script from a file. *)
let script_from_file ~filename =
  let chan = open_in filename in
  let result =
    Fun.protect
      ~finally:(fun () -> close_in chan)
      (fun () ->
        let lb = Sedlexing.Utf8.from_channel chan in
        Sedlexing.set_filename lb filename;
        script_from_lexbuf lb )
  in
  result

(** Parse a module from a lexing buffer. *)
let module_from_lexbuf = menhir_rule_from_lexbuf Menhir_parser.modul

(** Parse a module from a string. *)
let module_from_string s = module_from_lexbuf (Sedlexing.Utf8.from_string s)

(** Parse a module from a channel. *)
let module_from_channel c = module_from_lexbuf (Sedlexing.Utf8.from_channel c)

(** Parse a module from a file. *)
let module_from_file ~filename =
  let chan = open_in filename in
  let result =
    Fun.protect
      ~finally:(fun () -> close_in chan)
      (fun () ->
        let lb = Sedlexing.Utf8.from_channel chan in
        Sedlexing.set_filename lb filename;
        module_from_lexbuf lb )
  in
  result
