open Types
open Types.Simplified

let rec optimize_expr (exp : expr) : expr =
  match exp with
  | I32_const x :: I32_const y :: I_binop (S32, Add) :: tl ->
    optimize_expr (I32_const (Int32.add x y) :: tl)
  (* | ... -> ... *)
  | _ -> exp

let optimize_func (func : func) =
  let { type_f ; locals ; body ; id } = func in
  let body = optimize_expr body in

  { type_f; locals; body; id }

let optimize_runtime_func f =
  let { value; index} = f in
  match value with
  | Imported _ -> f
  | Local f ->
    let value = Local (optimize_func f) in
    { value; index}

let optimize_funcs funs =
  Named.map optimize_runtime_func funs

let modul (m : modul) : modul =
  let func = optimize_funcs m.func in
  { m with func }

        (* let simplified_module fmt (m : modul) : unit =
          Format.fprintf fmt
            "@[<hov 2>(simplified_module%a@ @[<hov 2>(func %a)@]@ @[<hov 2>(global \
             %a)@]@ @[<hov 2>(export func %a)@]@@ )@]"
            id m.id funcs m.func globals m.global (lst export) m.exports.func *)



(* module Optimize = (struct *)

(* module Arg = Simplified *)

  (* let rec optimise_expr (exp : Simplified.expr) : Simplified.expr =
    match exp with
    | Simplified.I32_const x :: Simplified.I32_const y :: Simplified. :: tl -> i32.const (x + y) :: optimise_expr tl
    | _ -> exp *)
    (* | i32.const x :: i32.const y :: i32.add :: tl -> i32.const (x + y) :: optimise_expr tl
    | ... *)

    (* expr = instr list *)

(*      type func =
    { type_f : block_type
    ; locals : param list
    ; body : expr
    ; id : string option
    }

    *)

(* end) *)
