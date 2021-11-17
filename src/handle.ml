let error msg =
  Format.eprintf "error: %s@." msg;
  exit 1

module I = Menhir_parser.MenhirInterpreter
module S = MenhirLib.General

let state checkpoint : int =
  match Lazy.force (I.stack checkpoint) with
  | S.Nil ->
    (* Hmm... The parser is in its initial state. Its number is
       usually 0. This is a BIG HACK. TEMPORARY *)
    0
  | S.Cons (Element (s, _, _, _), _) -> I.number s

let handle_syntax_error lexbuf _checkpoint =
  let message =
    "Syntax error"
    (*
    try W.Menhir_parser_errors.message (state checkpoint) with
    | Not_found -> "Syntax error"*)
  in
  Format.eprintf "%s %a\n%!" message Types.pp_pos
    (fst @@ Sedlexing.lexing_positions lexbuf);
  exit 1

let rec loop next_token lexbuf (checkpoint : Types.file I.checkpoint) =
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
  with
  | Lexer.LexError (pos, msg) ->
    Format.fprintf Format.err_formatter "lexing error : %s at %a%!" msg
      Types.pp_pos pos;
    exit 1

(*
let rec f x=f x

let f _=assert false

let f=Obj.magic

let f _=exit 1
   *)
