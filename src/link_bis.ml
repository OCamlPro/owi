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

module Env = struct
  type t = { globals : const IMap.t } [@@unboxed]

  let empty = { globals = IMap.empty }

  let add_global id const env = { globals = IMap.add id const env.globals }

  let get_global (env : t) id : Types.const =
    match IMap.find_opt id env.globals with
    | None -> failwith "unbound global"
    | Some v -> v
end

module Const_interp = struct
  open Types

  type env = Env.t

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
    | Global_get id -> Stack.push stack (Env.get_global env id)

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

let eval_globals ls globals : Env.t =
  S.Fields.fold
    (fun id global env ->
      let const = eval_global ls env global in
      let env = Env.add_global id const env in
      env )
    globals Env.empty

let populate_exports env (added_exports : S.index S.exports) exports : exports =
  let globals =
    List.fold_left
      (fun globals (export : _ S.export) ->
        let const = Env.get_global env export.id in
        StringMap.add export.name const globals )
      exports.globals added_exports.global
  in
  { globals }

let link_module (module_ : module_) (ls : link_state) : link_state =
  let env = eval_globals ls module_.global in
  let default = populate_exports env module_.exports ls.default in
  let by_module_exports = populate_exports env module_.exports empty_exports in
  let by_module =
    match module_.id with
    | None -> ls.by_module
    | Some name -> StringMap.add name by_module_exports ls.by_module
  in
  { modules = module_ :: ls.modules; by_module; default }
