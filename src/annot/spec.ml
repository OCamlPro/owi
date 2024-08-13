open Types
open Fmt
open Syntax

(*

unop ::= '-'                       ==> Neg

binop ::= '+'                      ==> Plus
  | '-'                            ==> Minus
  | '*'                            ==> Mult
  | '/'                            ==> Div

term ::= '(' pterm ')'             ==> pterm
  | i32                            ==> Int32 (Int32.of_string i32)                  if Option.is_some (Int32.of_string_opt i32)
  | ind                            ==> let* ind = parse_text_indice ind in Var ind  if Option.is_some (parse_text_id ind)
  | result                         ==> Result

pterm ::= 'i32' i32                ==> Int32 (Int32.of_string i32)
  | 'i64' i64                      ==> Int64 (Int64.of_string i64)
  | 'f32' f32                      ==> Float32 (Float32.of_string f32)
  | 'f64' f64                      ==> Float64 (Float64.of_string f64)
  | 'param' ind                    ==> let* ind = parse_indice ind in ParamVar ind
  | 'global' ind                   ==> let* ind = parse_indice ind in GlobalVar ind
  | 'binder' ind                   ==> let* ind = parse_indice ind in BinderVar ind
  | unop term_1                    ==> Unop (unop, term_1)
  | binop term_1 term_2            ==> BinOp (binop, term_1, term_2)

binpred ::= '>='                   ==> Ge
  | '>'                            ==> Gt
  | '<='                           ==> Le
  | '<'                            ==> Lt
  | '='                            ==> Eq
  | '!='                           ==> Neq

unconnect ::= '!'                  ==> Not

binconnect ::= '&&'                ==> And
  | '||'                           ==> Or
  | '==>'                          ==> Imply
  | '<==>'                         ==> Equiv

prop ::= '(' pprop ')'             ==> pprop
  | 'true'                         ==> Const true
  | 'false'                        ==> Const false

binder ::= 'forall'                ==> Forall
  | 'exists'                       ==> Exists

binder_type ::= 'i32'              ==> I32
  | 'i64'                          ==> I64
  | 'f32'                          ==> F32
  | 'f64'                          ==> F64

pprop ::= binpred term_1 term_2    ==> BinPred (binpred, term_1, term_2)
  | unconnect prop_1               ==> UnConnect (unconnect, prop_1)
  | binconnect prop_1 prop_2       ==> BinConnect (binconnect, prop_1, prop_2)
  | binder binder_type prop_1      ==> Binder (binder, binder_type, None, prop_1)
  | binder binder_type ind prop_1  ==> let* ind = (parse_text_id_result ind) in
                                          Binder (binder, binder_type, Some ind, prop_1)

*)

type nonrec binpred =
  | Ge
  | Gt
  | Le
  | Lt
  | Eq
  | Neq

type nonrec unconnect = Not

type nonrec binconnect =
  | And
  | Or
  | Imply
  | Equiv

type nonrec binder =
  | Forall
  | Exists

type nonrec binder_type = num_type

type nonrec unop =
  | Neg
  | CustomUnOp of string (* for testing purpose only *)

type nonrec binop =
  | Plus
  | Minus
  | Mult
  | Div
  | CustomBinOp of string (* for testing purpose only *)

type 'a term =
  | Int32 : Int32.t -> 'a term
  | Int64 : Int64.t -> 'a term
  | Float32 : Float32.t -> 'a term
  | Float64 : Float64.t -> 'a term
  | Var : text indice -> text term
  | ParamVar : 'a indice -> 'a term
  | GlobalVar : 'a indice -> 'a term
  | BinderVar : 'a indice -> 'a term
  | UnOp : unop * 'a term -> 'a term
  | BinOp : binop * 'a term * 'a term -> 'a term
  | Result : 'a term

type 'a prop =
  | Const : bool -> 'a prop
  | BinPred : binpred * 'a term * 'a term -> 'a prop
  | UnConnect : unconnect * 'a prop -> 'a prop
  | BinConnect : binconnect * 'a prop * 'a prop -> 'a prop
  | Binder : binder * binder_type * string option * 'a prop -> 'a prop

let pp_binpred fmt = function
  | Ge -> pf fmt "≥"
  | Gt -> pf fmt ">"
  | Le -> pf fmt "≤"
  | Lt -> pf fmt "<"
  | Eq -> pf fmt "="
  | Neq -> pf fmt "≠"

let pp_unconnect fmt = function Not -> pf fmt "¬"

let pp_binconnect fmt = function
  | And -> pf fmt "∧"
  | Or -> pf fmt "∨"
  | Imply -> pf fmt "⇒"
  | Equiv -> pf fmt "⇔"

let pp_binder fmt = function Forall -> pf fmt "∀" | Exists -> pf fmt "∃"

let pp_binder_type = pp_num_type

let pp_unop fmt = function
  | Neg -> pf fmt "-"
  | CustomUnOp c -> pf fmt "%a" string c

let pp_binop fmt = function
  | Plus -> pf fmt "+"
  | Minus -> pf fmt "-"
  | Mult -> pf fmt "*"
  | Div -> pf fmt "/"
  | CustomBinOp c -> pf fmt "%a" string c

let rec pp_term : type a. formatter -> a term -> unit =
 fun fmt -> function
  | Int32 i32 -> pf fmt "(i32 %i)" (Int32.to_int i32)
  | Int64 i64 -> pf fmt "(i64 %i)" (Int64.to_int i64)
  | Float32 f32 -> pf fmt "(f32 %a)" Float32.pp f32
  | Float64 f64 -> pf fmt "(f64 %a)" Float64.pp f64
  | Var ind -> pf fmt "%a" pp_indice ind
  | ParamVar ind -> pf fmt "(param %a)" pp_indice ind
  | GlobalVar ind -> pf fmt "(global %a)" pp_indice ind
  | BinderVar ind -> pf fmt "(binder %a)" pp_indice ind
  | UnOp (u, tm1) -> pf fmt "@[<hv 2>(%a@ %a)@]" pp_unop u pp_term tm1
  | BinOp (b, tm1, tm2) ->
    pf fmt "@[<hv 2>(%a@ %a@ %a)@]" pp_binop b pp_term tm1 pp_term tm2
  | Result -> pf fmt "result"

let rec pp_prop : type a. formatter -> a prop -> unit =
 fun fmt -> function
  | Const bool -> pf fmt "%a" Fmt.bool bool
  | BinPred (b, tm1, tm2) ->
    pf fmt "@[<hv 2>(%a@ %a@ %a)@]" pp_binpred b pp_term tm1 pp_term tm2
  | UnConnect (u, pr1) -> pf fmt "@[<hv 2>(%a@ %a)@]" pp_unconnect u pp_prop pr1
  | BinConnect (b, pr1, pr2) ->
    pf fmt "@[<hv 2>(%a@ %a@ %a)@]" pp_binconnect b pp_prop pr1 pp_prop pr2
  | Binder (b, bt, id_opt, pr1) -> (
    match id_opt with
    | Some id ->
      pf fmt "@[<hv 2>(%a %a:%a@ %a)@]" pp_binder b pp_id id pp_binder_type bt
        pp_prop pr1
    | None ->
      pf fmt "@[<hv 2>(%a %a@ %a)@]" pp_binder b pp_binder_type bt pp_prop pr1 )

let valid_text_indice_char = function
  | '0' .. '9'
  | 'a' .. 'z'
  | 'A' .. 'Z'
  | '!' | '#' | '$' | '%' | '&' | '\'' | '*' | '+' | '-' | '.' | '/' | ':' | '<'
  | '=' | '>' | '?' | '@' | '\\' | '^' | '_' | '`' | '|' | '~' ->
    true
  | _ -> false

let parse_text_id id =
  let len = String.length id in
  if len >= 2 then
    let hd = String.get id 0 in
    let tl = String.sub id 1 (len - 1) in
    if Char.equal hd '$' && String.for_all valid_text_indice_char id
    then Some tl
    else None
  else None

let parse_text_id_result id =
  let len = String.length id in
  if len >= 2 then
    let hd = String.get id 0 in
    let tl = String.sub id 1 (len - 1) in
    if Char.equal hd '$' && String.for_all valid_text_indice_char id
    then Ok tl
    else Error (`Invalid_text_indice id)
  else Error (`Invalid_text_indice id)

let parse_raw_id id = int_of_string id

let parse_text_indice id =
  match parse_text_id id with
  | Some id -> Ok (Text id)
  | None -> Error (`Invalid_text_indice id)

let parse_indice id =
  match (parse_text_id id, parse_raw_id id) with
  | Some id, _ -> Ok (Text id)
  | _, Some id -> Ok (Raw id)
  | _, _ -> Error (`Invalid_indice id)

let parse_binder_type =
  let open Sexp in
  function
  | Atom "i32" -> Ok I32
  | Atom "i64" -> Ok I64
  | Atom "f32" -> Ok F32
  | Atom "f64" -> Ok F64
  | bt -> Error (`Unknown_binder_type bt)

let rec parse_term =
  let open Sexp in
  function
  (* Int32 *)
  | Atom i32 when Option.is_some (Int32.of_string_opt i32) ->
    ok @@ Int32 (Int32.of_string i32)
  | List [ Atom "i32"; Atom i32 ] -> (
    match Int32.of_string_opt i32 with
    | Some i32 -> ok @@ Int32 i32
    | None -> Error (`Invalid_int32 i32) )
  (* Int64 *)
  | List [ Atom "i64"; Atom i64 ] -> (
    match Int64.of_string_opt i64 with
    | Some i64 -> ok @@ Int64 i64
    | None -> Error (`Invalid_int64 i64) )
  (* Float32 *)
  | List [ Atom "f32"; Atom f32 ] -> (
    match Float32.of_string_opt f32 with
    | Some f32 -> ok @@ Float32 f32
    | None -> Error (`Invalid_float32 f32) )
  (* Float64 *)
  | List [ Atom "f64"; Atom f64 ] -> (
    match Float64.of_string_opt f64 with
    | Some f64 -> ok @@ Float64 f64
    | None -> Error (`Invalid_float64 f64) )
  (* Var *)
  | Atom ind when Option.is_some (parse_text_id ind) ->
    let+ ind = parse_text_indice ind in
    Var ind
  (* ParamVar *)
  | List [ Atom "param"; Atom ind ] ->
    let+ ind = parse_indice ind in
    ParamVar ind
  (* GlobalVar *)
  | List [ Atom "global"; Atom ind ] ->
    let+ ind = parse_indice ind in
    GlobalVar ind
  (* BinderVar *)
  | List [ Atom "binder"; Atom ind ] ->
    let+ ind = parse_indice ind in
    BinderVar ind
  (* UnOp *)
  | List [ Atom "-"; tm1 ] ->
    let+ tm1 = parse_term tm1 in
    UnOp (Neg, tm1)
  | List [ Atom c; tm1 ] ->
    let+ tm1 = parse_term tm1 in
    UnOp (CustomUnOp c, tm1)
  (* BinOp *)
  | List [ Atom "+"; tm1; tm2 ] ->
    let* tm1 = parse_term tm1 in
    let+ tm2 = parse_term tm2 in
    BinOp (Plus, tm1, tm2)
  | List [ Atom "-"; tm1; tm2 ] ->
    let* tm1 = parse_term tm1 in
    let+ tm2 = parse_term tm2 in
    BinOp (Minus, tm1, tm2)
  | List [ Atom "*"; tm1; tm2 ] ->
    let* tm1 = parse_term tm1 in
    let+ tm2 = parse_term tm2 in
    BinOp (Mult, tm1, tm2)
  | List [ Atom "/"; tm1; tm2 ] ->
    let* tm1 = parse_term tm1 in
    let+ tm2 = parse_term tm2 in
    BinOp (Div, tm1, tm2)
  | List [ Atom c; tm1; tm2 ] ->
    let* tm1 = parse_term tm1 in
    let+ tm2 = parse_term tm2 in
    BinOp (CustomBinOp c, tm1, tm2)
  (* Result *)
  | Atom "result" -> ok Result
  (* Invalid *)
  | tm -> Error (`Unknown_term tm)

let rec parse_prop =
  let open Sexp in
  function
  (* Const *)
  | Atom "true" -> ok @@ Const true
  | Atom "false" -> ok @@ Const false
  (* BinPred *)
  | List [ Atom ">="; tm1; tm2 ] ->
    let* tm1 = parse_term tm1 in
    let+ tm2 = parse_term tm2 in
    BinPred (Ge, tm1, tm2)
  | List [ Atom ">"; tm1; tm2 ] ->
    let* tm1 = parse_term tm1 in
    let+ tm2 = parse_term tm2 in
    BinPred (Gt, tm1, tm2)
  | List [ Atom "<="; tm1; tm2 ] ->
    let* tm1 = parse_term tm1 in
    let+ tm2 = parse_term tm2 in
    BinPred (Le, tm1, tm2)
  | List [ Atom "<"; tm1; tm2 ] ->
    let* tm1 = parse_term tm1 in
    let+ tm2 = parse_term tm2 in
    BinPred (Lt, tm1, tm2)
  | List [ Atom "="; tm1; tm2 ] ->
    let* tm1 = parse_term tm1 in
    let+ tm2 = parse_term tm2 in
    BinPred (Eq, tm1, tm2)
  | List [ Atom "!="; tm1; tm2 ] ->
    let* tm1 = parse_term tm1 in
    let+ tm2 = parse_term tm2 in
    BinPred (Neq, tm1, tm2)
  (* UnConnect *)
  | List [ Atom "!"; pr1 ] ->
    let+ pr1 = parse_prop pr1 in
    UnConnect (Not, pr1)
  (* BinConnect *)
  | List [ Atom "&&"; pr1; pr2 ] ->
    let* pr1 = parse_prop pr1 in
    let+ pr2 = parse_prop pr2 in
    BinConnect (And, pr1, pr2)
  | List [ Atom "||"; pr1; pr2 ] ->
    let* pr1 = parse_prop pr1 in
    let+ pr2 = parse_prop pr2 in
    BinConnect (Or, pr1, pr2)
  | List [ Atom "==>"; pr1; pr2 ] ->
    let* pr1 = parse_prop pr1 in
    let+ pr2 = parse_prop pr2 in
    BinConnect (Imply, pr1, pr2)
  | List [ Atom "<==>"; pr1; pr2 ] ->
    let* pr1 = parse_prop pr1 in
    let+ pr2 = parse_prop pr2 in
    BinConnect (Equiv, pr1, pr2)
  (* Binder *)
  | List [ Atom "forall"; bt; pr1 ] ->
    let* bt = parse_binder_type bt in
    let+ pr1 = parse_prop pr1 in
    Binder (Forall, bt, None, pr1)
  | List [ Atom "forall"; bt; Atom ind; pr1 ] ->
    let* bt = parse_binder_type bt in
    let* ind = parse_text_id_result ind in
    let+ pr1 = parse_prop pr1 in
    Binder (Forall, bt, Some ind, pr1)
  | List [ Atom "exists"; bt; pr1 ] ->
    let* bt = parse_binder_type bt in
    let+ pr1 = parse_prop pr1 in
    Binder (Exists, bt, None, pr1)
  | List [ Atom "exists"; bt; Atom ind; pr1 ] ->
    let* bt = parse_binder_type bt in
    let* ind = parse_text_id_result ind in
    let+ pr1 = parse_prop pr1 in
    Binder (Exists, bt, Some ind, pr1)
  (* invalid *)
  | _ as pr -> Error (`Unknown_prop pr)
