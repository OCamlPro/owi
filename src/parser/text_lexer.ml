(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

open Sedlexing
open Text_parser

exception Empty_annotation_id

exception Empty_identifier

exception Illegal_character of string

exception Illegal_escape of string

exception Unclosed_annotation

exception Unclosed_comment

exception Unclosed_string

exception Unknown_operator of string

let illegal_character buf =
  let tok = Utf8.lexeme buf in
  raise @@ Illegal_character (Fmt.str "illegal character %S" tok)

let illegal_escape buf =
  let tok = Utf8.lexeme buf in
  raise @@ Illegal_escape (Fmt.str "illegal escape %S" tok)

let unknown_operator buf =
  let tok = Utf8.lexeme buf in
  raise @@ Unknown_operator (Fmt.str "unknown operator %S" tok)

let mk_string buf s =
  let b = Buffer.create (String.length s) in
  let i = ref 0 in
  while !i < String.length s do
    let c =
      if not @@ Char.equal s.[!i] '\\' then s.[!i]
      else
        match
          incr i;
          s.[!i]
        with
        | 'n' -> '\n'
        | 'r' -> '\r'
        | 't' -> '\t'
        | '\\' -> '\\'
        | '\'' -> '\''
        | '\"' -> '\"'
        | 'u' ->
          let j = !i + 2 in
          begin match String.index_from_opt s j '}' with
          | None -> (* TODO: is this the expected error ? *) illegal_escape buf
          | Some index ->
            i := index;
            let n =
              int_of_string_opt (Fmt.str "0x%s" (String.sub s j (!i - j)))
            in
            let n = match n with None -> assert false | Some n -> n in
            let bs = Wutf8.encode [ n ] in
            Buffer.add_substring b bs 0 (String.length bs - 1);
            bs.[String.length bs - 1]
          end
        | h ->
          incr i;
          if !i >= String.length s then illegal_escape buf;
          let str = Fmt.str "0x%c%c" h s.[!i] in
          begin match int_of_string_opt str with
          | None -> illegal_escape buf
          | Some n -> Char.chr n
          end
    in
    Buffer.add_char b c;
    incr i
  done;
  Buffer.contents b

let blank = [%sedlex.regexp? ' ' | '\t']

let newline = [%sedlex.regexp? '\r' | '\n' | "\r\n"]

let any_blank = [%sedlex.regexp? blank | newline]

let sign = [%sedlex.regexp? '+' | '-']

let digit = [%sedlex.regexp? '0' .. '9']

let hexdigit = [%sedlex.regexp? digit | 'a' .. 'f' | 'A' .. 'F']

let num = [%sedlex.regexp? digit, Star (Opt '_', digit)]

let hexnum = [%sedlex.regexp? hexdigit, Star (Opt '_', hexdigit)]

let hexfrac = [%sedlex.regexp? hexnum]

let frac = [%sedlex.regexp? num]

let float =
  [%sedlex.regexp?
    ( Opt sign, num, '.', Opt frac
    | Opt sign, num, Opt ('.', Opt frac), ('e' | 'E'), Opt sign, num
    | Opt sign, "0x", hexnum, '.', Opt hexfrac
    | Opt sign, "0x", hexnum, Opt ('.', Opt hexfrac), ('p' | 'P'), Opt sign, num
    | Opt sign, "inf"
    | Opt sign, "nan"
    | Opt sign, "nan:", "0x", hexnum )]

let nat = [%sedlex.regexp? num | "0x", hexnum]

let int = [%sedlex.regexp? sign, nat]

let num = [%sedlex.regexp? float | int | nat]

let id_char =
  [%sedlex.regexp?
    ( '0' .. '9'
    | 'a' .. 'z'
    | 'A' .. 'Z'
    | '!' | '#' | '$' | '%' | '&' | '\'' | '*' | '+' | '-' | '.' | '/' | ':'
    | '<' | '=' | '>' | '?' | '@' | '\\' | '^' | '_' | '`' | '|' | '~' )]

let string_elem = [%sedlex.regexp? Sub (any, "\"") | "\\\""]

let name = [%sedlex.regexp? "\"", Star string_elem, "\""]

let operator =
  [%sedlex.regexp? Plus ('0' .. '9' | 'a' .. 'z' | '.' | '_' | ':'), Star name]

let id = [%sedlex.regexp? "$", Plus id_char]

let bad_name = [%sedlex.regexp? name, Plus (name | id | operator)]

let bad_id = [%sedlex.regexp? id, Plus name]

let bad_num = [%sedlex.regexp? num, Plus id]

let annot_atom =
  [%sedlex.regexp?
    Plus id_char | num | name | ',' | ';' | '[' | ']' | '{' | '}']

let rec token buf =
  match%sedlex buf with
  | Plus any_blank -> token buf
  | bad_num | bad_id | bad_name -> unknown_operator buf
  | num -> NUM (Utf8.lexeme buf)
  | operator -> begin
    let operator = Utf8.lexeme buf in
    match Text_keywords.string_to_token operator with
    | None -> unknown_operator buf
    | Some v -> v
    end
  (* comment *)
  | ";;" ->
    single_comment buf;
    token buf
  | "(;" ->
    comment buf;
    token buf
  (* custom annotation *)
  | "(@", name ->
    let annotid = Utf8.lexeme buf in
    let annotid = String.sub annotid 3 (String.length annotid - 4) in
    let annotid = mk_string buf annotid in
    if String.equal "" annotid then raise Empty_annotation_id
    else begin
      annot buf;
      token buf
    end
  | "(@", Plus id_char ->
    annot buf;
    token buf
  | "(@" -> raise Empty_annotation_id
  (* 1 *)
  | "(" -> LPAR
  | ")" -> RPAR
  | "=" -> EQUAL
  (* other *)
  | id ->
    let id = Utf8.lexeme buf in
    let id = String.sub id 1 (String.length id - 1) in
    ID id
  | "$" -> raise Empty_identifier
  | name ->
    let name = Utf8.lexeme buf in
    let name = String.sub name 1 (String.length name - 2) in
    let name = mk_string buf name in
    NAME name
  | "\"", Star string_elem -> raise Unclosed_string
  | eof -> EOF
  (* | "" -> EOF *)
  | any -> unknown_operator buf
  | _ -> unknown_operator buf

and comment buf =
  match%sedlex buf with
  | ";)" -> ()
  | "(;" ->
    comment buf;
    comment buf
  | eof -> raise Unclosed_comment
  | any -> comment buf
  | _ -> assert false

and single_comment buf =
  match%sedlex buf with
  | newline -> ()
  | eof -> raise Unclosed_comment
  | any -> single_comment buf
  | _ -> assert false

and annot buf =
  match%sedlex buf with
  | Plus any_blank -> annot buf
  | ";;" ->
    single_comment buf;
    annot buf
  | "(;" ->
    comment buf;
    annot buf
  | "(" ->
    annot buf;
    annot buf
  | ")" -> ()
  | "\"", Star string_elem -> raise Unclosed_string
  | eof -> raise Unclosed_annotation
  | annot_atom -> annot buf
  | _ -> illegal_character buf

let lexer buf = Sedlexing.with_tokenizer token buf
