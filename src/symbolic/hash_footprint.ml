open Smtml
open Fmt

type t = int

let combine a b = (a * 31) + b

let rec hash_expr (e : Expr.t) : int =
  match Expr.view e with
  | Expr.Val v ->
      combine (Hashtbl.hash "Val") (Value.hash v)
  | Expr.Ptr { base; offset } ->
      let h = combine (Hashtbl.hash "Ptr") (Bitvector.hash base) in
      combine h (hash_expr offset)
  | Expr.Symbol sym ->
      let ty = Symbol.type_of sym in
      combine (Hashtbl.hash "Symbol") (Hashtbl.hash ty)
  | Expr.List es ->
      let h = Hashtbl.hash "List" in
      List.fold_left (fun acc e -> combine acc (hash_expr e)) h es
  | Expr.App (sym, args) ->
      let ty = Symbol.type_of sym in
      let h = combine (Hashtbl.hash "App") (Hashtbl.hash ty) in
      List.fold_left (fun acc e -> combine acc (hash_expr e)) h args
  | Expr.Unop (ty, op, e) ->
      let h = combine (Hashtbl.hash "Unop") (Ty.hash ty) in
      let h = combine h (Ty.Unop.hash op) in
      combine h (hash_expr e)
  | Expr.Binop (ty, op, e1, e2) ->
      let h = combine (Hashtbl.hash "Binop") (Ty.hash ty) in
      let h = combine h (Ty.Binop.hash op) in
      let h = combine h (hash_expr e1) in
      combine h (hash_expr e2)
  | Expr.Triop (ty, op, e1, e2, e3) ->
      let h = combine (Hashtbl.hash "Triop") (Ty.hash ty) in
      let h = combine h (Ty.Triop.hash op) in
      let h = combine h (hash_expr e1) in
      let h = combine h (hash_expr e2) in
      combine h (hash_expr e3)
  | Expr.Relop (ty, op, e1, e2) ->
      let h = combine (Hashtbl.hash "Relop") (Ty.hash ty) in
      let h = combine h (Ty.Relop.hash op) in
      let h = combine h (hash_expr e1) in
      combine h (hash_expr e2)
  | Expr.Cvtop (ty, op, e) ->
      let h = combine (Hashtbl.hash "Cvtop") (Ty.hash ty) in
      let h = combine h (Ty.Cvtop.hash op) in
      combine h (hash_expr e)
  | Expr.Naryop (ty, op, es) ->
      let h = combine (Hashtbl.hash "Naryop") (Ty.hash ty) in
      let h = combine h (Ty.Naryop.hash op) in
      List.fold_left (fun acc e -> combine acc (hash_expr e)) h es
  | Expr.Extract (e, high, low) ->
      let h = combine (Hashtbl.hash "Extract") (hash_expr e) in
      let h = combine h high in
      combine h low
  | Expr.Concat (e1, e2) ->
      let h = combine (Hashtbl.hash "Concat") (hash_expr e1) in
      combine h (hash_expr e2)
  | Expr.Binder (binder, vars, body) ->
      let h = combine (Hashtbl.hash "Binder") (Binder.hash binder) in
      let h_vars =
        List.fold_left
          (fun acc var ->
            match Expr.view var with
            | Expr.Symbol sym ->
                combine acc (Hashtbl.hash (Symbol.type_of sym))
            | _ ->
                combine acc (hash_expr var))
          h vars
      in
      combine h_vars (hash_expr body)

let of_expr e =
  let h = hash_expr e in
  Logs.debug (fun m -> m "Hash_footprint: computed hash %d" h);
  h
let equal a b = a = b
let hash a = a
let of_hash h = h

let pp fmt h = pf fmt "Hash(%d)" h
