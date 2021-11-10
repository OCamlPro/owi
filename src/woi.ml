let error msg =
  Format.eprintf "error: %s@." msg;
  exit 1

module I = Woi.Menhir_parser.MenhirInterpreter
module S = MenhirLib.General
module W = Woi

let state checkpoint : int =
  match Lazy.force (I.stack checkpoint) with
  | S.Nil ->
    (* Hmm... The parser is in its initial state. Its number is
       usually 0. This is a BIG HACK. TEMPORARY *)
    0
  | S.Cons (Element (s, _, _, _), _) -> I.number s

let handle_syntax_error lexbuf _checkpoint =
  let message = "Syntax error"(*
    try W.Menhir_parser_errors.message (state checkpoint) with
    | Not_found -> "Syntax error"*)
  in
  Format.eprintf "%s %a\n%!" message W.Types.pp_pos
    (fst @@ Sedlexing.lexing_positions lexbuf)

let rec loop next_token lexbuf (checkpoint : W.Types.file I.checkpoint) =
  match checkpoint with
  | I.InputNeeded _env ->
    let token = next_token () in
    let checkpoint = I.offer checkpoint token in
    loop next_token lexbuf checkpoint
  | I.Shifting _
  | I.AboutToReduce _ ->
    let checkpoint = I.resume checkpoint in
    loop next_token lexbuf checkpoint
  | I.HandlingError env -> handle_syntax_error lexbuf env
  | I.Accepted ast -> Format.printf "%a\n%!" W.Pp.file ast
  | I.Rejected ->
    (* Cannot happen as we stop at syntax error immediatly *)
    assert false

let process lexbuf =
  let lexer = W.Lexer.lexer lexbuf in
  try
    loop lexer lexbuf
      (W.Menhir_parser.Incremental.file
         (fst @@ Sedlexing.lexing_positions lexbuf) )
  with
  | W.Lexer.LexError (pos, msg) ->
    Format.fprintf Format.err_formatter "lexing error : %s at %a%!" msg
      W.Types.pp_pos pos

let () =
  if Array.length Sys.argv <> 2 then
    error (Format.sprintf "usage: %s <file>" Sys.argv.(0));

  let file = Sys.argv.(1) in

  if not @@ Sys.file_exists file then
    error (Format.sprintf "file `%s` doesn't exist" file);

  let chan = open_in file in

  let lexbuf = Sedlexing.Utf8.from_channel chan in

  process lexbuf;

  close_in chan
