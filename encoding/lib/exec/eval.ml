open Core
open Ast

type config = {
  code : Ast.t list;
  smap : (string, Types.expr_type) Hashtbl.t;
  pc : Expression.t list;
  solver : Batch.t;
}

let step (c : config) : config =
  let { code; smap; solver; pc } = c in
  let i = List.hd_exn code in
  let code', pc' =
    match i with
    | Declare x ->
        Hashtbl.add_exn smap ~key:(Symbol.to_string x) ~data:(Symbol.type_of x);
        (List.tl_exn code, pc)
    | Assert e -> (List.tl_exn code, e :: pc)
    | CheckSat ->
        if Batch.check_sat solver pc then printf "sat\n" else printf "unsat\n";
        (List.tl_exn code, pc)
    | GetModel ->
        let model = Batch.find_model solver pc in
        let assignments =
          List.map model ~f:(fun (s, v) ->
              let x = Symbol.to_string s
              and t = Types.string_of_type (Symbol.type_of s)
              and v' = Value.to_string v in
              sprintf "  (%s %s %s)" x t v')
        in
        printf "(model\n%s)" (String.concat ~sep:"\n" assignments);
        (List.tl_exn code, pc)
  in
  { c with code = code'; pc = pc' }

let rec eval (c : config) : config =
  match c.code with [] -> c | _ -> eval (step c)

let start (prog : Ast.t list) : unit =
  let c =
    {
      code = prog;
      smap = Hashtbl.create (module String);
      solver = Batch.create ();
      pc = [];
    }
  in
  ignore (eval c)
