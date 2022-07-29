open Types
module S = Simplify_bis

type module_ = S.result

module StringMap = Map.Make (String)

type exports = { globals : const StringMap.t }

type link_state =
  { modules : module_ list
  ; by_module : exports StringMap.t
  ; default : exports
  }

let empty_exports = { globals = StringMap.empty }

let empty_state =
  { modules = []; by_module = StringMap.empty; default = empty_exports }

module Index = struct
  type t = S.index

  let compare = compare
end

module IMap = Map.Make (Index)

module Const_interp = struct
  open Types

  type env = { globals : const IMap.t } [@@unboxed]

  let get_global env id : Types.const =
    match IMap.find_opt id env.globals with
    | None -> failwith "unbound global"
    | Some v -> v

  let exec_ibinop stack nn (op : Const.ibinop) =
    match nn with
    | S32 ->
      let (n1, n2), stack = Stack.pop2_i32 stack in
      Stack.push_i32 stack
        (let open Int32 in
        match op with Add -> add n1 n2 | Sub -> sub n1 n2 | Mul -> mul n1 n2)
    | S64 ->
      let (n1, n2), stack = Stack.pop2_i64 stack in
      Stack.push_i64 stack
        (let open Int64 in
        match op with Add -> add n1 n2 | Sub -> sub n1 n2 | Mul -> mul n1 n2)

  let exec_instr (env : env) (stack : Stack.t) (instr : Const.instr) =
    match instr with
    | I32_const n -> Stack.push_i32 stack n
    | I64_const n -> Stack.push_i64 stack n
    | F32_const f -> Stack.push_f32 stack f
    | F64_const f -> Stack.push_f64 stack f
    | I_binop (nn, op) -> exec_ibinop stack nn op
    | Ref_null t -> Stack.push stack (Const_null t)
    | Global_get id -> Stack.push stack (get_global env id)

  let exec_expr env (e : Const.expr) : const =
    let stack = List.fold_left (exec_instr env) Stack.empty e in
    match stack with
    | [] -> failwith "const expr returning zero values"
    | _ :: _ :: _ -> failwith "const expr returning more than one value"
    | [ result ] -> result
end

let load_global (ls : link_state) (import : S.global_import S.imp) : const =
  match StringMap.find import.module_ ls.by_module with
  | exception Not_found -> failwith ("unbound module " ^ import.module_)
  | exports -> (
    match StringMap.find import.name exports.globals with
    | exception Not_found -> failwith ("unbound name " ^ import.name)
    | v -> v )

let eval_global ls env
    (global : ((S.index, Const.expr) global', S.global_import) S.runtime) :
    const =
  match global with
  | S.Local global -> Const_interp.exec_expr env global.init
  | S.Imported import -> load_global ls import
