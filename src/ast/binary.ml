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
  ; init : binary expr
  ; id : string option
  }

type data_mode =
  | Data_passive
  (* TODO: Data_active binary+const expr*)
  | Data_active of int * binary expr

type data =
  { id : string option
  ; init : string
  ; mode : data_mode
  }

type elem_mode =
  | Elem_passive
  (* TODO: Elem_active binary+const expr*)
  | Elem_active of int option * binary expr
  | Elem_declarative

type elem =
  { id : string option
  ; typ : binary ref_type (* TODO: init : binary+const expr*)
  ; init : binary expr list
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
    ; table : (binary table, binary table_type) Runtime.t array
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

  (** Look for an imported function index, adding it if not already imported. *)
  let add_import_if_not_present ~modul_name ~func_name ~desc m =
    match find_imported_func_index ~modul_name ~func_name m with
    | Some i -> (Types.Raw i, m)
    | None ->
      let f =
        Runtime.Imported
          { Imported.modul = modul_name
          ; name = func_name
          ; assigned_name = None
          ; desc
          }
      in
      let func = Array.append m.func [| f |] in
      let m = { m with func } in
      let i = Array.length func - 1 in
      (Types.Raw i, m)

  (** Functions *)

  (** Add a function [f] to a module [m] and returns the module and the index of
      the added function. *)
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
end
