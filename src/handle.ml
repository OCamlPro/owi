let error msg =
  Format.eprintf "error: %s@." msg;
  exit 1

module I = Menhir_parser.MenhirInterpreter
module S = MenhirLib.General

let handle_syntax_error lexbuf _checkpoint =
  failwith
  @@ Format.asprintf "syntax error: %a" Pp.pos
       (fst @@ Sedlexing.lexing_positions lexbuf)

let rec loop next_token lexbuf (checkpoint : Types.file I.checkpoint) =
  match checkpoint with
  | I.InputNeeded _env ->
    let token = next_token () in
    let checkpoint = I.offer checkpoint token in
    loop next_token lexbuf checkpoint
  | I.Shifting _ | I.AboutToReduce _ ->
    let checkpoint = I.resume checkpoint in
    loop next_token lexbuf checkpoint
  | I.HandlingError env -> handle_syntax_error lexbuf env
  | I.Accepted ast -> ast
  | I.Rejected ->
    (* Cannot happen as we stop at syntax error immediatly *)
    assert false

let process lexbuf =
  let lexer = Lexer.lexer lexbuf in
  try
    loop lexer lexbuf
      (Menhir_parser.Incremental.file
         (fst @@ Sedlexing.lexing_positions lexbuf) )
  with Lexer.LexError (pos, msg) ->
    failwith @@ Format.asprintf "lexing error : %s at %a%!" msg Pp.pos pos
