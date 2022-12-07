(** Module providing functions to parse a wasm script from various kind of
    inputs. *)

(** Parse a script from a lexing buffer. *)
let from_lexbuf =
  Log.debug "parsing...@\n";
  let parser =
    MenhirLib.Convert.Simplified.traditional2revised Menhir_parser.script
  in
  fun buf ->
    let provider () =
      let tok = Lexer.token buf in
      let start, stop = Sedlexing.lexing_positions buf in
      (tok, start, stop)
    in
    try Ok (parser provider) with
    | Menhir_parser.Error ->
      let start, _stop = Sedlexing.lexing_positions buf in
      Log.debug "file %s, line %i, char %i@." start.pos_fname
        (start.pos_lnum + 1)
        (start.pos_cnum - start.pos_bol);
      Error "unexpected token"
    | Lexer.Error (_pos, _msg) -> Error "unknown operator"
    | Failure msg -> Error msg

(** Parse a script from a string. *)
let from_string s = from_lexbuf (Sedlexing.Utf8.from_string s)

(** Parse a script from a channel. *)
let from_channel c = from_lexbuf (Sedlexing.Utf8.from_channel c)

(** Parse a script from a file. *)
let from_file ~filename =
  let chan = open_in filename in
  let result =
    Fun.protect
      ~finally:(fun () -> close_in chan)
      (fun () -> from_lexbuf (Sedlexing.Utf8.from_channel chan))
  in
  result
