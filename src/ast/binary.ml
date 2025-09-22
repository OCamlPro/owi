(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Types

(** named export *)
type export =
  { name : string
  ; id : int
  }

(** named exports of a module *)
type exports =
  { global : export list
  ; mem : export list
  ; table : export list
  ; func : export list
  }

type global =
  { typ : binary global_type (* TODO: init : binary+const expr*)
  ; init : binary expr Annotated.t
  ; id : string option
  }

type data_mode =
  | Data_passive
  (* TODO: Data_active binary+const expr*)
  | Data_active of int * binary expr Annotated.t

type data =
  { id : string option
  ; init : string
  ; mode : data_mode
  }

type elem_mode =
  | Elem_passive
  (* TODO: Elem_active binary+const expr*)
  | Elem_active of int option * binary expr Annotated.t
  | Elem_declarative

type elem =
  { id : string option
  ; typ : ref_type (* TODO: init : binary+const expr*)
  ; init : binary expr Annotated.t list
  ; mode : elem_mode
  }

type custom =
  | Uninterpreted of string
  | From_annot of binary Annot.annot

module Module = struct
  type t =
    { id : string option
    ; types : binary type_def array
    ; global : (global, binary global_type) Runtime.t array
    ; table : (binary table, table_type) Runtime.t array
    ; mem : (mem, limits) Runtime.t array
    ; func : (binary func, binary block_type) Runtime.t array
        (* TODO: switch to func_type *)
    ; elem : elem array
    ; data : data array
    ; exports : exports
    ; start : int option
    ; custom : custom list
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
    ; exports = { global = []; mem = []; table = []; func = [] }
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
    let update_idx (Raw idx as raw : Types.binary Types.indice) :
      Types.binary Types.indice =
      if idx >= i then Raw (idx + 1) else raw
    in

    let rec handle_instr instr =
      Annotated.map
        (function
          | Types.Call idx -> Types.Call (update_idx idx)
          | Types.Return_call idx -> Types.Return_call (update_idx idx)
          | Types.Ref_func idx -> Types.Ref_func (update_idx idx)
          | Types.Block (id, typ, body) ->
            let body = handle_expr body in
            Types.Block (id, typ, body)
          | Types.Loop (id, typ, body) ->
            let body = handle_expr body in
            Types.Loop (id, typ, body)
          | Types.If_else (id, typ, true_branch, false_branch) ->
            let true_branch = handle_expr true_branch in
            let false_branch = handle_expr false_branch in
            Types.If_else (id, typ, true_branch, false_branch)
          | instr ->
            (* TODO: make this match non fragile *)
            instr )
        instr
    and handle_expr expr =
      Annotated.map (fun expr -> List.map handle_instr expr) expr
    in
    let update_function = function
      | Runtime.Imported _ as f -> f
      | Runtime.Local f ->
        let body = handle_expr f.Types.body in
        Runtime.Local { f with body }
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
        (fun elem ->
          let init = List.map handle_expr elem.init in
          { elem with init } )
        m.elem
    in
    let global =
      Array.map
        (function
          | Runtime.Imported _ as v -> v
          | Local (global : global) ->
            let init = handle_expr global.init in
            Local { global with init } )
        m.global
    in

    let start =
      match m.start with
      | None -> None
      | Some idx ->
        let (Raw start) = update_idx (Raw idx) in
        Some start
    in

    let exports =
      let func =
        List.map
          (fun export ->
            let id : int = (export : export).id in
            let (Raw id) = update_idx (Raw id) in
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
      | Imported i -> Some i.desc

  (** Exports *)

  (** Return the first function exported as [name] if it exists. Return [None]
      otherwise.*)
  let find_exported_func_from_name name m =
    List.find_opt
      (function { name = name'; _ } -> String.equal name name')
      m.exports.func

  (** Imports *)

  (** Return the index of a function imported from a given [modul_name] and
      [func_name] if it exists. Return [None] otherwise. *)
  let find_imported_func_index ~modul_name ~func_name m =
    Array.find_index
      (function
        | Runtime.Imported { Imported.modul; name; assigned_name = _; desc = _ }
          ->
          String.equal modul_name modul && String.equal func_name name
        | Local _ -> false )
      m.func

  (** Finds the index of the last imported function. Will be `~-1` if there are
      no imported functions. *)
  let find_last_import_index m =
    let _i, last =
      Array.fold_left
        (fun (i, last) -> function
          | Runtime.Imported _ -> (succ i, i) | Runtime.Local _ -> (succ i, last) )
        (0, ~-1) m.func
    in
    last

  (** Look for an imported function index, adding it if not already imported. *)
  let add_import_if_not_present ~modul_name ~func_name ~desc m =
    match find_imported_func_index ~modul_name ~func_name m with
    | Some _i -> m
    | None ->
      let f =
        Runtime.Imported
          { Imported.modul = modul_name
          ; name = func_name
          ; assigned_name = None
          ; desc
          }
      in

      let idx = find_last_import_index m + 1 in

      insert_func_at_idx f m idx
end
