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
      Log.debug "File %s, line %i, character %i:@." start.pos_fname
        start.pos_lnum
        (start.pos_cnum - start.pos_bol);
      Error "unexpected token"
    | Lexer.Error (pos, msg) ->
      if !Log.debug_on then begin
        let file_line =
          let cpos = pos.pos_cnum - pos.pos_bol in
          Printf.sprintf "File \"%s\", line %i, character %i:" pos.pos_fname
            pos.pos_lnum cpos
        in
        let msg = Printf.sprintf "Error: Lexing error %s" msg in
        Log.debug "%s\n%s\n" file_line msg
      end;
      Error "unknown operator"
    | Failure msg ->
      if !Log.debug_on then begin
        let msg = Printf.sprintf "Error: Lexing error %s" msg in
        Log.debug "%s\n" msg
      end;
      Error msg

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
      (fun () ->
        let lb = Sedlexing.Utf8.from_channel chan in
        Sedlexing.set_filename lb filename;
        from_lexbuf lb )
  in
  result
