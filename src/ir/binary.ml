(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Fmt

let sp ppf () = Fmt.char ppf ' '

(* identifiers *)

type indice = int

let pp_indice fmt i = int fmt i

let pp_indice_not0 fmt i = if i <> 0 then Fmt.pf fmt " %d" i

(** Structure *)

(** Types *)

type block_type =
  (* TODO: inline this *)
  | Bt_raw of (indice option * Text.func_type)

(* wrap printer to print a space before a non empty list *)
(* TODO or make it an optional arg of pp_list? *)
let with_space_list printer fmt l =
  match l with [] -> () | _l -> pf fmt " %a" printer l

let pp_block_type fmt = function
  | Bt_raw (_ind, (pt, rt)) ->
    pf fmt "%a%a"
      (with_space_list Text.pp_param_type)
      pt
      (with_space_list Text.pp_result_type)
      rt

let pp_block_type_opt fmt = function
  | None -> ()
  | Some bt -> pp_block_type fmt bt

(** Instructions *)

type instr =
  (* Numeric Instructions *)
  | I32_const of Int32.t
  | I64_const of Int64.t
  | F32_const of Float32.t
  | F64_const of Float64.t
  | V128_const of Concrete_v128.t
  | I_unop of Text.nn * Text.iunop
  | F_unop of Text.nn * Text.funop
  | I_binop of Text.nn * Text.ibinop
  | F_binop of Text.nn * Text.fbinop
  | V_ibinop of Text.ishape * Text.vibinop
  | I_testop of Text.nn * Text.itestop
  | I_relop of Text.nn * Text.irelop
  | F_relop of Text.nn * Text.frelop
  | I_extend8_s of Text.nn
  | I_extend16_s of Text.nn
  | I64_extend32_s
  | I32_wrap_i64
  | I64_extend_i32 of Text.sx
  | I_trunc_f of Text.nn * Text.nn * Text.sx
  | I_trunc_sat_f of Text.nn * Text.nn * Text.sx
  | F32_demote_f64
  | F64_promote_f32
  | F_convert_i of Text.nn * Text.nn * Text.sx
  | I_reinterpret_f of Text.nn * Text.nn
  | F_reinterpret_i of Text.nn * Text.nn
  (* Reference instructions *)
  | Ref_null of Text.heap_type
  | Ref_is_null
  | Ref_func of indice
  (* Parametric instructions *)
  | Drop
  | Select of Text.val_type list option
  (* Variable instructions *)
  | Local_get of indice
  | Local_set of indice
  | Local_tee of indice
  | Global_get of indice
  | Global_set of indice
  (* Table instructions *)
  | Table_get of indice
  | Table_set of indice
  | Table_size of indice
  | Table_grow of indice
  | Table_fill of indice
  | Table_copy of indice * indice
  | Table_init of indice * indice
  | Elem_drop of indice
  (* Memory instructions *)
  | I_load of indice * Text.nn * Text.memarg
  | F_load of indice * Text.nn * Text.memarg
  | I_store of indice * Text.nn * Text.memarg
  | F_store of indice * Text.nn * Text.memarg
  | I_load8 of indice * Text.nn * Text.sx * Text.memarg
  | I_load16 of indice * Text.nn * Text.sx * Text.memarg
  | I64_load32 of indice * Text.sx * Text.memarg
  | I_store8 of indice * Text.nn * Text.memarg
  | I_store16 of indice * Text.nn * Text.memarg
  | I64_store32 of indice * Text.memarg
  | Memory_size of indice
  | Memory_grow of indice
  | Memory_fill of indice
  | Memory_copy of indice * indice
  | Memory_init of indice * indice
  | Data_drop of indice
  (* Control instructions *)
  | Nop
  | Unreachable
  | Block of string option * block_type option * expr Annotated.t
  | Loop of string option * block_type option * expr Annotated.t
  | If_else of
      string option * block_type option * expr Annotated.t * expr Annotated.t
  | Br of indice
  | Br_if of indice
  | Br_table of indice array * indice
  | Return
  | Return_call of indice
  | Return_call_indirect of indice * block_type
  | Return_call_ref of block_type
  | Call of indice
  | Call_indirect of indice * block_type
  | Call_ref of indice
  (* extern *)
  | Extern_externalize
  | Extern_internalize

and expr = instr Annotated.t list

let pp_newline ppf () = pf ppf "@\n"

let rec pp_instr ~short fmt = function
  | I32_const i -> pf fmt "i32.const %ld" i
  | I64_const i -> pf fmt "i64.const %Ld" i
  | F32_const f -> pf fmt "f32.const %a" Float32.pp f
  | F64_const f -> pf fmt "f64.const %a" Float64.pp f
  | V128_const f -> pf fmt "v128.const %a" Concrete_v128.pp f
  | I_unop (n, op) -> pf fmt "i%a.%a" Text.pp_nn n Text.pp_iunop op
  | F_unop (n, op) -> pf fmt "f%a.%a" Text.pp_nn n Text.pp_funop op
  | I_binop (n, op) -> pf fmt "i%a.%a" Text.pp_nn n Text.pp_ibinop op
  | F_binop (n, op) -> pf fmt "f%a.%a" Text.pp_nn n Text.pp_fbinop op
  | V_ibinop (shape, op) ->
    pf fmt "%a.%a" Text.pp_ishape shape Text.pp_vibinop op
  | I_testop (n, op) -> pf fmt "i%a.%a" Text.pp_nn n Text.pp_itestop op
  | I_relop (n, op) -> pf fmt "i%a.%a" Text.pp_nn n Text.pp_irelop op
  | F_relop (n, op) -> pf fmt "f%a.%a" Text.pp_nn n Text.pp_frelop op
  | I_extend8_s n -> pf fmt "i%a.extend8_s" Text.pp_nn n
  | I_extend16_s n -> pf fmt "i%a.extend16_s" Text.pp_nn n
  | I64_extend32_s -> pf fmt "i64.extend32_s"
  | I32_wrap_i64 -> pf fmt "i32.wrap_i64"
  | I64_extend_i32 sx -> pf fmt "i64.extend_i32_%a" Text.pp_sx sx
  | I_trunc_f (n, n', sx) ->
    pf fmt "i%a.trunc_f%a_%a" Text.pp_nn n Text.pp_nn n' Text.pp_sx sx
  | I_trunc_sat_f (n, n', sx) ->
    pf fmt "i%a.trunc_sat_f%a_%a" Text.pp_nn n Text.pp_nn n' Text.pp_sx sx
  | F32_demote_f64 -> pf fmt "f32.demote_f64"
  | F64_promote_f32 -> pf fmt "f64.promote_f32"
  | F_convert_i (n, n', sx) ->
    pf fmt "f%a.convert_i%a_%a" Text.pp_nn n Text.pp_nn n' Text.pp_sx sx
  | I_reinterpret_f (n, n') ->
    pf fmt "i%a.reinterpret_f%a" Text.pp_nn n Text.pp_nn n'
  | F_reinterpret_i (n, n') ->
    pf fmt "f%a.reinterpret_i%a" Text.pp_nn n Text.pp_nn n'
  | Ref_null t -> pf fmt "ref.null %a" Text.pp_heap_type t
  | Ref_is_null -> pf fmt "ref.is_null"
  | Ref_func fid -> pf fmt "ref.func %a" pp_indice fid
  | Drop -> pf fmt "drop"
  | Select vt -> begin
    match vt with
    | None -> pf fmt "select"
    | Some vt -> pf fmt "select (%a)" Text.pp_result_type vt
    (* TODO: are the parens needed ? *)
  end
  | Local_get id -> pf fmt "local.get %a" pp_indice id
  | Local_set id -> pf fmt "local.set %a" pp_indice id
  | Local_tee id -> pf fmt "local.tee %a" pp_indice id
  | Global_get id -> pf fmt "global.get %a" pp_indice id
  | Global_set id -> pf fmt "global.set %a" pp_indice id
  | Table_get id -> pf fmt "table.get %a" pp_indice id
  | Table_set id -> pf fmt "table.set %a" pp_indice id
  | Table_size id -> pf fmt "table.size %a" pp_indice id
  | Table_grow id -> pf fmt "table.grow %a" pp_indice id
  | Table_fill id -> pf fmt "table.fill %a" pp_indice id
  | Table_copy (id, id') -> pf fmt "table.copy %a %a" pp_indice id pp_indice id'
  | Table_init (tid, eid) ->
    pf fmt "table.init %a %a" pp_indice tid pp_indice eid
  | Elem_drop id -> pf fmt "elem.drop %a" pp_indice id
  | I_load (id, n, memarg) ->
    pf fmt "i%a.load%a %a" Text.pp_nn n pp_indice_not0 id Text.pp_memarg memarg
  | F_load (id, n, memarg) ->
    pf fmt "f%a.load%a %a" Text.pp_nn n pp_indice_not0 id Text.pp_memarg memarg
  | I_store (id, n, memarg) ->
    pf fmt "i%a.store%a %a" Text.pp_nn n pp_indice_not0 id Text.pp_memarg memarg
  | F_store (id, n, memarg) ->
    pf fmt "f%a.store%a %a" Text.pp_nn n pp_indice_not0 id Text.pp_memarg memarg
  | I_load8 (id, n, sx, memarg) ->
    pf fmt "i%a.load8_%a%a %a" Text.pp_nn n Text.pp_sx sx pp_indice_not0 id
      Text.pp_memarg memarg
  | I_load16 (id, n, sx, memarg) ->
    pf fmt "i%a.load16_%a%a %a" Text.pp_nn n Text.pp_sx sx pp_indice_not0 id
      Text.pp_memarg memarg
  | I64_load32 (id, sx, memarg) ->
    pf fmt "i64.load32_%a%a %a" Text.pp_sx sx pp_indice_not0 id Text.pp_memarg
      memarg
  | I_store8 (id, n, memarg) ->
    pf fmt "i%a.store8%a %a" Text.pp_nn n pp_indice_not0 id Text.pp_memarg
      memarg
  | I_store16 (id, n, memarg) ->
    pf fmt "i%a.store16%a %a" Text.pp_nn n pp_indice_not0 id Text.pp_memarg
      memarg
  | I64_store32 (id, memarg) ->
    pf fmt "i64.store32%a %a" pp_indice_not0 id Text.pp_memarg memarg
  | Memory_size id -> pf fmt "memory.size%a" pp_indice_not0 id
  | Memory_grow id -> pf fmt "memory.grow%a" pp_indice_not0 id
  | Memory_fill id -> pf fmt "memory.fill%a" pp_indice_not0 id
  | Memory_copy (0, 0) -> pf fmt "memory.copy"
  | Memory_copy (id1, id2) ->
    pf fmt "memory.copy %a %a" pp_indice id1 pp_indice id2
  | Memory_init (memidx, dataidx) ->
    pf fmt "memory.init%a %a" pp_indice_not0 memidx pp_indice dataidx
  | Data_drop id -> pf fmt "data.drop %a" pp_indice id
  | Nop -> pf fmt "nop"
  | Unreachable -> pf fmt "unreachable"
  | Block (id, bt, e) ->
    if short then pf fmt "block%a%a" Text.pp_id_opt id pp_block_type_opt bt
    else
      pf fmt "(block%a%a@\n  @[<v>%a@])" Text.pp_id_opt id pp_block_type_opt bt
        (pp_expr ~short) e
  | Loop (id, bt, e) ->
    if short then pf fmt "loop%a%a" Text.pp_id_opt id pp_block_type_opt bt
    else
      pf fmt "(loop%a%a@\n  @[<v>%a@])" Text.pp_id_opt id pp_block_type_opt bt
        (pp_expr ~short) e
  | If_else (id, bt, e1, e2) ->
    let pp_else fmt e =
      Annotated.iter
        (function
          | [] -> ()
          | _ -> pf fmt "@\n(else@\n  @[<v>%a@]@\n)" (pp_expr ~short) e )
        e
    in
    if short then pf fmt "if%a%a" Text.pp_id_opt id pp_block_type_opt bt
    else
      pf fmt "(if%a%a@\n  @[<v>(then@\n  @[<v>%a@]@\n)%a@]@\n)" Text.pp_id_opt
        id pp_block_type_opt bt (pp_expr ~short) e1 pp_else e2
  | Br id -> pf fmt "br %a" pp_indice id
  | Br_if id -> pf fmt "br_if %a" pp_indice id
  | Br_table (ids, id) ->
    pf fmt "br_table %a %a" (array ~sep:sp pp_indice) ids pp_indice id
  | Return -> pf fmt "return"
  | Return_call id -> pf fmt "return_call %a" pp_indice id
  | Return_call_indirect (tbl_id, ty_id) ->
    pf fmt "return_call_indirect %a %a" pp_indice tbl_id pp_block_type ty_id
  | Return_call_ref ty_id -> pf fmt "return_call_ref %a" pp_block_type ty_id
  | Call id -> pf fmt "call %a" pp_indice id
  | Call_indirect (tbl_id, ty_id) ->
    pf fmt "call_indirect %a %a" pp_indice tbl_id pp_block_type ty_id
  | Call_ref ty_id -> pf fmt "call_ref %a" pp_indice ty_id
  | Extern_externalize -> pf fmt "extern.externalize"
  | Extern_internalize -> pf fmt "extern.internalize"

and pp_expr ~short fmt instrs =
  Annotated.iter
    (fun instrs ->
      list ~sep:pp_newline
        (fun fmt i -> Annotated.iter (pp_instr ~short fmt) i)
        fmt instrs )
    instrs

let rec iter_expr f (e : expr Annotated.t) =
  Annotated.iter (List.iter (iter_instr f)) e

and iter_instr f instr =
  Annotated.iter f instr;
  Annotated.iter
    (function
      | I32_const _ | I64_const _ | F32_const _ | F64_const _ | V128_const _
      | I_unop (_, _)
      | F_unop (_, _)
      | I_binop (_, _)
      | F_binop (_, _)
      | V_ibinop (_, _)
      | I_testop (_, _)
      | I_relop (_, _)
      | F_relop (_, _)
      | I_extend8_s _ | I_extend16_s _ | I64_extend32_s | I32_wrap_i64
      | I64_extend_i32 _
      | I_trunc_f (_, _, _)
      | I_trunc_sat_f (_, _, _)
      | F32_demote_f64 | F64_promote_f32
      | F_convert_i (_, _, _)
      | I_reinterpret_f (_, _)
      | F_reinterpret_i (_, _)
      | Ref_null _ | Ref_is_null | Ref_func _ | Drop | Select _ | Local_get _
      | Local_set _ | Local_tee _ | Global_get _ | Global_set _ | Table_get _
      | Table_set _ | Table_size _ | Table_grow _ | Table_fill _
      | Table_copy (_, _)
      | Table_init (_, _)
      | Elem_drop _
      | I_load (_, _, _)
      | F_load (_, _, _)
      | I_store (_, _, _)
      | F_store (_, _, _)
      | I_load8 (_, _, _, _)
      | I_load16 (_, _, _, _)
      | I64_load32 (_, _, _)
      | I_store8 (_, _, _)
      | I_store16 (_, _, _)
      | I64_store32 _ | Memory_size _ | Memory_grow _ | Memory_fill _
      | Memory_copy _ | Memory_init _ | Data_drop _ | Nop | Unreachable | Br _
      | Br_if _
      | Br_table (_, _)
      | Return | Return_call _
      | Return_call_indirect (_, _)
      | Return_call_ref _ | Call _
      | Call_indirect (_, _)
      | Call_ref _ | Extern_externalize | Extern_internalize ->
        ()
      | Block (_, _, e) | Loop (_, _, e) -> iter_expr f e
      | If_else (_, _, e1, e2) ->
        iter_expr f e1;
        iter_expr f e2 )
    instr

module Func = struct
  type t =
    { type_f : block_type
    ; locals : Text.param list
    ; body : expr Annotated.t
    ; id : string option
    }
end

(* Modules *)

(** named export *)
module Export = struct
  type t =
    { name : string
    ; id : int
    }
end

module Global = struct
  type t =
    { typ : Text.Global.Type.t (* TODO: init : binary+const expr*)
    ; init : expr Annotated.t
    ; id : string option
    }
end

module Data = struct
  module Mode = struct
    type t =
      | Passive
      (* TODO: Data_active binary+const expr*)
      | Active of int * expr Annotated.t
  end

  type t =
    { id : string option
    ; init : string
    ; mode : Mode.t
    }
end

module Elem = struct
  module Mode = struct
    type t =
      | Passive
      | Declarative
      (* TODO: Elem_active binary+const expr*)
      | Active of int option * expr Annotated.t
  end

  type t =
    { id : string option
    ; typ : Text.ref_type (* TODO: init : binary+const expr*)
    ; init : expr Annotated.t list
    ; mode : Mode.t
    }
end

module Custom = struct
  type t = Uninterpreted of string
end

module Module = struct
  module Exports = struct
    type t =
      { global : Export.t Array.t
      ; mem : Export.t Array.t
      ; table : Export.t Array.t
      ; func : Export.t Array.t
      }
  end

  type t =
    { id : string option
    ; types : Text.Typedef.t array
    ; global : (Global.t, Text.Global.Type.t) Origin.t array
    ; table : (Text.Table.t, Text.Table.Type.t) Origin.t array
    ; mem : (Text.Mem.t, Text.limits) Origin.t array
    ; func : (Func.t, block_type) Origin.t array (* TODO: switch to func_type *)
    ; elem : Elem.t array
    ; data : Data.t array
    ; exports : Exports.t
    ; start : int option
    ; custom : Custom.t list
    }

  let empty =
    { id = None
    ; types = [||]
    ; global = [||]
    ; table = [||]
    ; mem = [||]
    ; func = [||]
    ; elem = [||]
    ; data = [||]
    ; exports = { global = [||]; mem = [||]; table = [||]; func = [||] }
    ; start = None
    ; custom = []
    }

  (** Functions *)

  (** Insert a function [f] to a module [m] at index [i] and returns the module.
      It will update all function indices accordingly. *)
  let insert_func_at_idx ?(update_function_itself = true) f m i =
    (* TODO: we should also update elements and everything... *)
    (*
    Log.warn (fun m ->
      m "insert_func_at_idx is still incomplete and you may run into issues" );
    *)
    let update_idx idx = if idx >= i then idx + 1 else idx in

    let rec handle_instr instr =
      Annotated.map
        (function
          | Call idx -> Call (update_idx idx)
          | Return_call idx -> Return_call (update_idx idx)
          | Ref_func idx -> Ref_func (update_idx idx)
          | Block (id, typ, body) ->
            let body = handle_expr body in
            Block (id, typ, body)
          | Loop (id, typ, body) ->
            let body = handle_expr body in
            Loop (id, typ, body)
          | If_else (id, typ, true_branch, false_branch) ->
            let true_branch = handle_expr true_branch in
            let false_branch = handle_expr false_branch in
            If_else (id, typ, true_branch, false_branch)
          | instr ->
            (* TODO: make this match non fragile *)
            instr )
        instr
    and handle_expr expr =
      Annotated.map (fun expr -> List.map handle_instr expr) expr
    in
    let update_function = function
      | Origin.Imported _ as f -> f
      | Origin.Local (f : Func.t) ->
        let body = handle_expr f.body in
        Origin.Local { f with body }
    in
    let func =
      Array.init
        (Array.length m.func + 1)
        (fun j ->
          if i = j then if update_function_itself then update_function f else f
          else begin
            update_function @@ if i < j then m.func.(j - 1) else m.func.(j)
          end )
    in
    let elem =
      Array.map
        (fun (elem : Elem.t) ->
          let init = List.map handle_expr elem.init in
          { elem with init } )
        m.elem
    in
    let global =
      Array.map
        (function
          | Origin.Imported _ as v -> v
          | Local (global : Global.t) ->
            let init = handle_expr global.init in
            Local { global with init } )
        m.global
    in

    let start = Option.map update_idx m.start in

    let exports =
      let func =
        Array.map
          (fun export ->
            let id = update_idx (export : Export.t).id in
            { export with id } )
          m.exports.func
      in
      { m.exports with func }
    in

    { m with func; elem; start; global; exports }

  (** Add a function [f] at the end of a module [m] and returns the module and
      the index of the added function. *)
  let add_func f m =
    let len = Array.length m.func in
    let func =
      Array.init
        (Array.length m.func + 1)
        (fun i -> if i = len then f else m.func.(i))
    in

    ({ m with func }, len)

  (** Return the type of the function at index [id]. *)
  let get_func_type id m =
    if id >= Array.length m.func then None
    else
      match m.func.(id) with
      | Local f -> Some f.type_f
      | Imported i -> Some i.typ

  (** Exports *)

  (** Return the first function exported as [name] if it exists. Return [None]
      otherwise.*)
  let find_exported_func_from_name name m =
    Array.find_opt
      (function { Export.name = name'; _ } -> String.equal name name')
      m.exports.func

  (** Imports *)

  (** Return the index of a function imported from a given [modul_name] and
      [func_name] if it exists. Return [None] otherwise. *)
  let find_imported_func_index ~modul_name ~func_name m =
    Array.find_index
      (function
        | Origin.Imported
            { Origin.modul_name = modul_name'
            ; name
            ; assigned_name = _
            ; typ = _
            } ->
          String.equal modul_name modul_name' && String.equal func_name name
        | Local _ -> false )
      m.func

  (** Finds the index of the last imported function. Will be `~-1` if there are
      no imported functions. *)
  let find_last_import_index m =
    let _i, last =
      Array.fold_left
        (fun (i, last) -> function
          | Origin.Imported _ -> (succ i, i) | Origin.Local _ -> (succ i, last) )
        (0, ~-1) m.func
    in
    last

  (** Look for an imported function index, adding it if not already imported. *)
  let add_import_if_not_present ~modul_name ~func_name ~typ m =
    match find_imported_func_index ~modul_name ~func_name m with
    | Some _i -> m
    | None ->
      let f =
        Origin.imported ~modul_name ~name:func_name ~assigned_name:None ~typ
      in

      let idx = find_last_import_index m + 1 in

      insert_func_at_idx f m idx
end
