open Types
open Types.Simplified
open Value

let rec optimize_expr (exp : expr) : expr =
  match exp with
  | ((I32_const _ | I64_const _) as x) :: ((I32_const _ | I64_const _) as y) :: I_binop (nn, op) :: tl ->
    begin try
      let result = Interpret.exec_ibinop [(Value.of_instr y); (Value.of_instr x)] nn op in
      begin
        match result with
        | [I32 _ | I64 _ as result ] -> optimize_expr (Value.to_instr result :: tl)
        | _ -> assert false
      end
    with Trap _ -> x ::  optimize_expr (y :: I_binop (nn, op) :: tl)
    end
  | ((F32_const _ | F64_const _) as x) :: ((F32_const _ | F64_const _) as y) :: F_binop (nn, op) :: tl ->
      let result = Interpret.exec_fbinop [(Value.of_instr y); (Value.of_instr x)] nn op in
      begin
        match result with
        | [F32 _ | F64 _ as result ] -> optimize_expr (Value.to_instr result :: tl)
        | _ -> assert false
      end

(* TODO ? another binary/unary ops *)

  | (I32_const _ | I64_const _ | F32_const _ | F64_const _) :: Drop :: tl ->
      optimize_expr tl
  | Local_set x :: Local_get y :: tl when x = y ->
      optimize_expr (Local_tee x :: tl)
  | Br _ as br :: _tl -> [br]
  (* | I32_const c :: (Br_if _ as brif) :: tl -> (* TODO : KO *)
    begin match c with
      | 0l -> optimize_expr tl
      | _ -> [brif]
    end *)
  | Return :: _tl -> [Return]
  | Return_call _ as rc :: _tl -> [rc]
  | Return_call_indirect _ as rci :: _tl -> [rci]
  | Nop :: tl -> optimize_expr tl
  | Block (n, bt, e) :: tl ->
    begin match optimize_expr e with
    | [] -> optimize_expr tl
    | oe -> Block (n, bt, oe) :: optimize_expr tl
    end
  | Loop (n, bt, e) :: tl ->
    begin match optimize_expr e with
    | [] -> optimize_expr tl
    | oe -> Loop (n, bt, oe) :: optimize_expr tl
    end
  | If_else (n, bt, e1, e2) :: tl ->
    begin match optimize_expr e1, optimize_expr e2 with
    | [], [] -> Drop :: optimize_expr tl
    | oe1, oe2 -> If_else (n, bt, oe1, oe2) :: optimize_expr tl
    end
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

let locals_used_in_func (locals : param list) (nb_params : int) (func_body : expr) : param list * expr =
  let loc_hashtbl = locals_func func_body
  in
  let remove_local (idx : int) (body : expr) : expr =
    let rec aux_instr (i : instr) : instr =
      match i with
      | Local_get ind when nb_params <= idx && idx < ind -> Local_get (ind-1)
      | Local_set ind when nb_params <= idx && idx < ind -> Local_set (ind-1)
      | Local_tee ind when nb_params <= idx && idx < ind -> Local_tee (ind-1)
      | Block (m, t, e) -> Block (m, t, aux_expr e)
      | Loop (m, t, e) -> Loop (m, t, aux_expr e)
      | If_else (m, t, e1, e2) -> If_else (m, t, aux_expr e1, aux_expr e2)
      | _ -> i
    and aux_expr (e : expr) : expr =
      List.map aux_instr e
    in
    aux_expr body
  in
  let loop (idx, pl, body : int * param list * expr) (p : param) : int * param list * expr =
    match Hashtbl.find_opt loc_hashtbl idx with
    | None -> idx+1, (if nb_params <= idx then pl else List.append pl [p]), remove_local idx body
    | Some _ -> idx+1, List.append pl [p], body
  in
    let _, l, b = List.fold_left loop (0, [], func_body) locals in
      l, b

let optimize_func (func : func) =
  let { type_f ; locals ; body ; id } = func in
  let (pt, _) = type_f in
  let nb_params = List.length pt in
  let locals, body = locals_used_in_func locals nb_params body in
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
  Log.debug "optimizing   ...@\n";
  let func = optimize_funcs m.func in
  { m with func }
