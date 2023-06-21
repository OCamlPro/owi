%{
open Core
open Value
open Expression
open Types

let varmap = Hashtbl.create (module String)

let add_bind x t = Hashtbl.set varmap ~key:x ~data:t
let get_bind x = Hashtbl.find_exn varmap x

%}
%token LPAREN
%token RPAREN
%token ASSERT 
%token DECLARE_FUN CHECK_SAT GET_MODEL
(*%token HOLE*)
%token EOF

%token <int> NUM
%token <float> DEC
%token <bool> BOOL
%token <string> STR
%token <string> SYMBOL
%token <Types.unop> UNARY
%token <Types.binop> BINARY
%token <Types.triop> TERNARY 
%token <Types.relop> RELOP
%token <Types.cvtop> CVTOP
%token <Types.expr_type> TYPE

%start <Ast.t list> script
%%

script: list(stmt) EOF { $1 }

stmt:
  | LPAREN; DECLARE_FUN; x = SYMBOL; t = TYPE; RPAREN
    {
      add_bind x t;
      Ast.Declare (Symbol.mk_symbol t x) 
    }
  | LPAREN; ASSERT; e = s_expr; RPAREN { Ast.Assert e }
  | LPAREN; CHECK_SAT; RPAREN { Ast.CheckSat }
  | LPAREN; GET_MODEL; RPAREN { Ast.GetModel }
  ;

s_expr:
  | x = SYMBOL { mk_symbol_s (get_bind x) x }
  | c = spec_constant { Val c }
  | LPAREN; op = UNARY; e = s_expr; RPAREN { Unop (op, e) }
  | LPAREN; op = BINARY; e1 = s_expr; e2 = s_expr; RPAREN { Binop (op, e1, e2) }
  | LPAREN; op = TERNARY; e1 = s_expr; e2 = s_expr; e3 = s_expr; RPAREN
    { Triop (op, e1, e2, e3) }
  | LPAREN; op = CVTOP; e = s_expr; RPAREN { Cvtop (op, e) }
  | LPAREN; op = RELOP; e1 = s_expr; e2 = s_expr; RPAREN { Relop (op, e1, e2) }
  ;

spec_constant :
  | NUM { Int $1 }
  | DEC { Real $1 }
  | STR { Str $1 }
  | BOOL { Bool $1 }
  | LPAREN; TYPE; NUM; RPAREN 
    {
      match $2 with
      | `I32Type -> Num (I32 (Int32.of_int_trunc $3))
      | `I64Type -> Num (I64 (Int64.of_int $3))
      | _ -> failwith "invalid integer type"
    }
  | LPAREN; TYPE; DEC; RPAREN 
    {
      match $2 with
      | `F32Type -> Num (F32 (Int32.bits_of_float $3))
      | `F64Type -> Num (F64 (Int64.bits_of_float $3))
      | _ -> failwith "invalid integer type"
    }
  ;
