{
open Types
open Lexing
open Parser

exception SyntaxError of string

let intop  =
  let tbl = Hashtbl.create 21 in
  List.to_seq [
    ("neg", UNARY (Int I.Neg));
    ("add", BINARY (Int I.Add));
    ("sub", BINARY (Int I.Sub));
    ("div", BINARY (Int I.Div));
    ("mul", BINARY (Int I.Mul));
    ("shl", BINARY (Int I.Shl));
    ("shr_a", BINARY (Int I.ShrA));
    ("shr_u", BINARY (Int I.ShrL));
    ("pow", BINARY (Int I.Pow));
    ("and", BINARY (Int I.And));
    ("or", BINARY (Int I.Or));
    ("xor", BINARY (Int I.Xor));
    ("eq", RELOP (Int I.Eq));
    ("ne", RELOP (Int I.Ne));
    ("lt", RELOP (Int I.Lt));
    ("le", RELOP (Int I.Le));
    ("gt", RELOP (Int I.Gt));
    ("ge", RELOP (Int I.Ge));
    ("to_string", CVTOP (Int I.ToString));
    ("of_string", CVTOP (Int I.OfString));
    ("reinterpret_real", CVTOP (Int I.ReinterpretReal));
  ]
  |> Hashtbl.add_seq tbl;
  tbl

let realop =
  let tbl = Hashtbl.create 20 in
  List.to_seq [
    ("neg", UNARY (Real R.Neg));
    ("abs", UNARY (Real R.Abs));
    ("sqrt", UNARY (Real R.Sqrt));
    ("nearest", UNARY (Real R.Nearest));
    ("is_nan", UNARY (Real R.IsNan));
    ("add", BINARY (Real R.Add));
    ("sub", BINARY (Real R.Sub));
    ("div", BINARY (Real R.Div));
    ("mul", BINARY (Real R.Mul));
    ("rem", BINARY (Real R.Rem));
    ("min", BINARY (Real R.Min));
    ("max", BINARY (Real R.Max));
    ("eq", RELOP (Real R.Eq));
    ("ne", RELOP (Real R.Ne));
    ("lt", RELOP (Real R.Lt));
    ("le", RELOP (Real R.Le));
    ("gt", RELOP (Real R.Gt));
    ("ge", RELOP (Real R.Ge));
    ("reinterpret_int", CVTOP (Real R.ReinterpretInt));
    ("to_string", CVTOP (Real R.ToString));
    ("of_string", CVTOP (Real R.OfString));
  ]
  |> Hashtbl.add_seq tbl;
  tbl

let boolop = 
  let tbl = Hashtbl.create 6 in
  List.to_seq [
    ("not", UNARY (Bool B.Not));
    ("and", BINARY (Bool B.And));
    ("or", BINARY (Bool B.Or));
    ("xor", BINARY (Bool B.Xor));
    ("eq", RELOP (Bool B.Eq));
    ("ne", RELOP (Bool B.Ne));
    ("ite", TERNARY (Bool B.ITE));
  ]
  |> Hashtbl.add_seq tbl;
  tbl

let strop =
  let tbl = Hashtbl.create 6 in
  List.to_seq [
    ("len", UNARY (Str S.Len));
    ("nth", BINARY (Str S.Nth));
    ("++", BINARY (Str S.Concat));
    ("sub", TERNARY (Str S.SubStr));
    ("eq", RELOP (Str S.Eq));
    ("ne", RELOP (Str S.Ne));
  ]
  |> Hashtbl.add_seq tbl;
  tbl

let i32op =
  let tbl = Hashtbl.create 32 in
  List.to_seq [
    ("clz", UNARY (I32 I32.Clz));
    ("not", UNARY (I32 I32.Not));
    ("add", BINARY (I32 I32.Add));
    ("sub", BINARY (I32 I32.Sub));
    ("div_s", BINARY (I32 I32.DivS));
    ("div_u", BINARY (I32 I32.DivU));
    ("and", BINARY (I32 I32.And));
    ("or", BINARY (I32 I32.Or));
    ("xor", BINARY (I32 I32.Xor));
    ("mul", BINARY (I32 I32.Mul));
    ("shl", BINARY (I32 I32.Shl));
    ("shr_s", BINARY (I32 I32.ShrS));
    ("shr_u", BINARY (I32 I32.ShrU));
    ("rem_s", BINARY (I32 I32.RemS));
    ("rem_u", BINARY (I32 I32.RemU));
    ("eq", RELOP (I32 I32.Eq));
    ("ne", RELOP (I32 I32.Ne));
    ("lt_u", RELOP (I32 I32.LtU));
    ("lt_s", RELOP (I32 I32.LtS));
    ("le_u", RELOP (I32 I32.LeU));
    ("le_s", RELOP (I32 I32.LeS));
    ("gt_u", RELOP (I32 I32.GtU));
    ("gt_s", RELOP (I32 I32.GtS));
    ("ge_u", RELOP (I32 I32.GeU));
    ("ge_s", RELOP (I32 I32.GeS));
    ("ge_s", RELOP (I32 I32.GeS));
    ("trunc_f32_s",  CVTOP (I32 I32.TruncSF32));
    ("trunc_f32_u", CVTOP (I32 I32.TruncUF32));
    ("trunc_f64_s", CVTOP (I32 I32.TruncSF64));
    ("trunc_f64_u", CVTOP (I32 I32.TruncUF64));
    ("reinterpret_f32", CVTOP (I32 I32.ReinterpretFloat));
    ("wrap_i64", CVTOP (I32 I32.WrapI64));
  ]
  |> Hashtbl.add_seq tbl;
  tbl

let i64op =
  let tbl = Hashtbl.create 33 in
  List.to_seq [
    ("clz", UNARY (I64 I64.Clz));
    ("not", UNARY (I64 I64.Not));
    ("add", BINARY (I64 I64.Add));
    ("sub", BINARY (I64 I64.Sub));
    ("div_s", BINARY (I64 I64.DivS));
    ("div_u", BINARY (I64 I64.DivU));
    ("and", BINARY (I64 I64.And));
    ("or", BINARY (I64 I64.Or));
    ("xor", BINARY (I64 I64.Xor));
    ("mul", BINARY (I64 I64.Mul));
    ("shl", BINARY (I64 I64.Shl));
    ("shr_s", BINARY (I64 I64.ShrS));
    ("shr_u", BINARY (I64 I64.ShrU));
    ("rem_s", BINARY (I64 I64.RemS));
    ("rem_u", BINARY (I64 I64.RemU));
    ("eq", RELOP (I64 I64.Eq));
    ("ne", RELOP (I64 I64.Ne));
    ("lt_u", RELOP (I64 I64.LtU));
    ("lt_s", RELOP (I64 I64.LtS));
    ("le_u", RELOP (I64 I64.LeU));
    ("le_s", RELOP (I64 I64.LeS));
    ("gt_u", RELOP (I64 I64.GtU));
    ("gt_s", RELOP (I64 I64.GtS));
    ("ge_u", RELOP (I64 I64.GeU));
    ("ge_s", RELOP (I64 I64.GeS));
    ("ge_s", RELOP (I64 I64.GeS));
    ("trunc_f32_s",  CVTOP (I64 I64.TruncSF32));
    ("trunc_f32_u", CVTOP (I64 I64.TruncUF32));
    ("trunc_f64_s", CVTOP (I64 I64.TruncSF64));
    ("trunc_f64_u", CVTOP (I64 I64.TruncUF64));
    ("reinterpret_f64", CVTOP (I64 I64.ReinterpretFloat));
    ("extend_i32_s", CVTOP (I64 I64.ExtendSI32));
    ("extend_i32_u", CVTOP (I64 I64.ExtendUI32));
  ]
  |> Hashtbl.add_seq tbl;
  tbl

let f32op =
  let tbl = Hashtbl.create 23 in
  List.to_seq [
    ("neg", UNARY (F32 F32.Neg));
    ("abs", UNARY (F32 F32.Abs));
    ("sqrt", UNARY (F32 F32.Sqrt));
    ("nearest",UNARY (F32 F32.Nearest) );
    ("is_nan", UNARY (F32 F32.IsNan));
    ("add", BINARY (F32 F32.Add));
    ("sub", BINARY (F32 F32.Sub));
    ("mul", BINARY (F32 F32.Mul));
    ("div", BINARY (F32 F32.Div));
    ("min", BINARY (F32 F32.Min));
    ("max", BINARY (F32 F32.Max));
    ("rem", BINARY (F32 F32.Rem));
    ("eq", RELOP (F32 F32.Eq));
    ("ne", RELOP (F32 F32.Ne));
    ("lt", RELOP (F32 F32.Lt));
    ("le", RELOP (F32 F32.Le));
    ("gt", RELOP (F32 F32.Gt));
    ("ge", RELOP (F32 F32.Ge));
    ("convert_i32_s", CVTOP (F32 F32.ConvertSI32));
    ("convert_i32_u", CVTOP (F32 F32.ConvertUI32));
    ("convert_i64_s", CVTOP (F32 F32.ConvertSI32));
    ("demote_f64", CVTOP (F32 F32.DemoteF64));
    ("reinterpret_i32", CVTOP (F32 F32.ReinterpretInt));
  ]
  |> Hashtbl.add_seq tbl;
  tbl

let f64op =
  let tbl = Hashtbl.create 23 in
  List.to_seq [
    ("neg", UNARY (F64 F64.Neg));
    ("abs", UNARY (F64 F64.Abs));
    ("sqrt", UNARY (F64 F64.Sqrt));
    ("nearest",UNARY (F64 F64.Nearest) );
    ("is_nan", UNARY (F64 F64.IsNan));
    ("add", BINARY (F64 F64.Add));
    ("sub", BINARY (F64 F64.Sub));
    ("mul", BINARY (F64 F64.Mul));
    ("div", BINARY (F64 F64.Div));
    ("min", BINARY (F64 F64.Min));
    ("max", BINARY (F64 F64.Max));
    ("rem", BINARY (F64 F64.Rem));
    ("eq", RELOP (F64 F64.Eq));
    ("ne", RELOP (F64 F64.Ne));
    ("lt", RELOP (F64 F64.Lt));
    ("le", RELOP (F64 F64.Le));
    ("gt", RELOP (F64 F64.Gt));
    ("ge", RELOP (F64 F64.Ge));
    ("convert_i32_s", CVTOP (F64 F64.ConvertSI32));
    ("convert_i32_u", CVTOP (F64 F64.ConvertUI32));
    ("convert_i64_s", CVTOP (F64 F64.ConvertSI32));
    ("promote_f32", CVTOP (F64 F64.PromoteF32));
    ("reinterpret_i64", CVTOP (F64 F64.ReinterpretInt));
  ]
  |> Hashtbl.add_seq tbl;
  tbl

let token_of_op t op =
  let tbl =
    match t with
    | "int" -> intop
    | "real" -> realop
    | "bool" -> boolop
    | "str" -> strop
    | "i32" -> i32op
    | "i64" -> i64op
    | "f32" -> f32op
    | "f64" -> f64op
    | _ -> assert false
  in
  Hashtbl.find tbl op

let type_of = function
  | "int" -> `IntType
  | "real" -> `RealType
  | "bool" -> `BoolType
  | "str" -> `StrType
  | "i32" -> `I32Type
  | "i64" -> `I64Type
  | "f32" -> `F32Type
  | "f64" -> `F64Type
  | _ -> assert false

let keywords =
  let tbl = Hashtbl.create 3 in
  List.to_seq [
    ("assert", ASSERT);
    ("check-sat", CHECK_SAT);
    ("declare-fun", DECLARE_FUN);
    ("get-model", GET_MODEL);
  ]
  |> Hashtbl.add_seq tbl;
  tbl

let error msg = raise (SyntaxError msg)

}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

let digit = ['0'-'9']
let character = ['a'-'z' 'A'-'Z']
let numeral = '0' | '-'? [ '1'-'9' ] digit*
let decimal = numeral '.' '0'* numeral
let hexadec = "#x" (['a'-'f' 'A'-'F'] | digit)+
let binary = "#b" ('0' | '1')+
let bool = "true" | "false"

let symbols = ['~''!''@''$''%''^''&''*''_''-''+''=''<''>''.''?''/']
let symbol = (character | symbols) (character | digit | symbols)*
(* TODO: Quoted symbols: |symbol| *)

let ixx = "i32" | "i64"
let fxx = "f32" | "f64"
let op_t = "int" | "real" | "bool" | "str" | ixx | fxx

rule token = parse
  | '(' { LPAREN }
  | ')' { RPAREN }

  | numeral as s { NUM (Core.Int.of_string s) }
  | decimal as s { DEC (Core.Float.of_string s) }
  | bool as s { BOOL (Core.Bool.of_string s) }
  | hexadec { failwith "TODO: Lexer(hexadec)" }
  | binary { failwith "TODO: Lexer(binary)" }
  | '"' { string (Buffer.create 17) lexbuf }

  | (op_t as t)"."(symbol as op) { token_of_op t op }
  | (op_t as t) { TYPE (type_of t)  }

  | symbol as x { try Hashtbl.find keywords x with Not_found -> SYMBOL x }

  | ';' { comment lexbuf }
  | white { token lexbuf }
  | newline { new_line lexbuf; token lexbuf }
  | eof { EOF }

  | _ { error ("Unexpected char: " ^ Lexing.lexeme lexbuf) }

and comment = parse
  | newline { new_line lexbuf; token lexbuf }
  | _ { comment lexbuf }

and string buf = parse
  | '"' { STR (Buffer.contents buf) }
  | '"' '"' { Buffer.add_char buf '"'; string buf lexbuf }
  | [^ '"']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf); string buf lexbuf }
  | eof { error "nonterminated string" }
  | _ { error ("illegal string char: " ^ Lexing.lexeme lexbuf) }

