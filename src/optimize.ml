open Types
open Types.Simplified

let rec optimize_expr (exp : expr) : expr =
  match exp with
  | I32_const x :: I32_const y :: I_binop (S32, Add) :: tl ->
      optimize_expr (I32_const (Int32.add x y) :: tl)
  | I32_const x :: I32_const y :: I_binop (S32, Sub) :: tl ->
      optimize_expr (I32_const (Int32.sub x y) :: tl)
  | I32_const x :: I32_const y :: I_binop (S32, Mul) :: tl ->
      optimize_expr (I32_const (Int32.mul x y) :: tl)
  | I32_const x :: I32_const y :: I_binop (S32, Div U) :: tl ->
      optimize_expr (I32_const (Int32.div x y) :: tl)
  | I64_const x :: I64_const y :: I_binop (S64, Add) :: tl ->
      optimize_expr (I64_const (Int64.add x y) :: tl)
  | I64_const x :: I64_const y :: I_binop (S64, Sub) :: tl ->
      optimize_expr (I64_const (Int64.sub x y) :: tl)
  | I64_const x :: I64_const y :: I_binop (S64, Mul) :: tl ->
      optimize_expr (I64_const (Int64.mul x y) :: tl)
  | I64_const x :: I64_const y :: I_binop (S64, Div U) :: tl ->
      optimize_expr (I64_const (Int64.div x y) :: tl)
  (* | I32_const (Int32.zero) ::  :: tl -> EQZ
    optimize_expr (I32_const (Int32.add x y) :: tl) *)
  | Local_set x :: Local_get y :: tl when x = y ->
      optimize_expr (Local_tee x :: tl)
  | Block (n, bt, e) :: tl -> Block (n, bt, optimize_expr e) :: optimize_expr tl
  | Loop (n, bt, e) :: tl -> Loop (n, bt, optimize_expr e) :: optimize_expr tl
  | If_else (n, bt, e1, e2) :: tl -> If_else (n, bt, optimize_expr e1, optimize_expr e2) :: optimize_expr tl
  | hd :: tl -> hd :: optimize_expr tl
  | [] -> []
  
let locals_func (body : expr) : (int, unit) Hashtbl.t =
  let locals_hashtbl : (int, unit) Hashtbl.t = Hashtbl.create 10 in
  let rec aux_instr (i : instr) : unit =
    match i with
    | Local_get ind | Local_set ind | Local_tee ind ->
        Hashtbl.add locals_hashtbl ind ()
    | Block (_, _, e) -> aux_expr e
    | Loop (_, _, e) -> aux_expr e
    | If_else (_, _, e1, e2) -> aux_expr e1; aux_expr e2
    | _ -> ()
  and aux_expr (e : expr) : unit =
    List.iter aux_instr e
  in
  aux_expr body;
  locals_hashtbl

let locals_used_in_func (locals : param list) (func_body : expr) (func_id : string option) : param list =
  let loc_hashtbl = locals_func func_body
  in
  let str_func_id : string =
    match func_id with
    | None -> ""
    | Some s -> s
  in
  let str_idx_param (idx : int) (p : param) : string  =
    let str_opt, _ = p in
      match str_opt with
      | None -> string_of_int idx
      | Some s -> s
  in
  let local_find (idx : int) (p : param) : unit =
    match Hashtbl.find_opt loc_hashtbl idx with
    | None -> raise (Failure ("function " ^ str_func_id ^ ": local " ^ (str_idx_param idx p) ^ " not used"))
    | Some _ -> ()
  in
    List.iteri (fun idx p -> local_find idx p) locals;
    locals

let optimize_func (func : func) =
  let { type_f ; locals ; body ; id } = func in
  let body = optimize_expr body in
  let locals = locals_used_in_func locals body id in
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
