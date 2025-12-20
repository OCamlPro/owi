(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Text
open Syntax

type env =
  { start : bool
  ; declared_memory : bool
  ; funcs : bool
  ; tables : bool
  ; globals : bool
  }

let empty_env () =
  { start = false
  ; declared_memory = false
  ; funcs = false
  ; tables = false
  ; globals = false
  }

let modul m =
  Log.info (fun m -> m "checking     ...");
  let add_global, global_exists =
    let seen = Hashtbl.create 512 in
    let add_global = function
      | None -> Ok ()
      | Some id ->
        if Hashtbl.mem seen id then Error (`Duplicate_global id)
        else Ok (Hashtbl.replace seen id ())
    in
    let global_exists id = Hashtbl.mem seen id in
    (add_global, global_exists)
  in
  let add_table, get_table =
    let cnt = ref 0 in
    let names2ids = Hashtbl.create 512 in
    let seen = Hashtbl.create 512 in
    let add_table name ty =
      match name with
      | None ->
        Hashtbl.replace seen !cnt ty;
        incr cnt;
        Ok ()
      | Some name ->
        if Hashtbl.mem names2ids name then Error (`Duplicate_table name)
        else (
          Hashtbl.replace names2ids name !cnt;
          Hashtbl.replace seen !cnt ty;
          incr cnt;
          Ok () )
    in
    let get_table name =
      match name with
      | None -> begin
        match Hashtbl.find_opt seen 0 with
        | None -> assert false
        | Some ty -> Ok ty
      end
      | Some (Text name) -> begin
        match Hashtbl.find_opt names2ids name with
        | None -> Error (`Unknown_table (Text name))
        | Some id -> begin
          match Hashtbl.find_opt seen id with
          | None -> assert false
          | Some ty -> Ok ty
        end
      end
      | Some (Raw id) -> begin
        match Hashtbl.find_opt seen id with
        | None -> Error (`Unknown_table (Raw id))
        | Some ty -> Ok ty
      end
    in
    (add_table, get_table)
  in
  let elem_check_type env (elemnull, elemty) mode explicit_typ =
    match mode with
    | Text.Elem.Mode.(Passive | Declarative) -> Ok env
    | Text.Elem.Mode.Active (id, _) ->
      let* _, (tabnull, tabty) = get_table id in
      (* Only if elem_ty is explicit, otherwise it can be inferred *)
      if
        (not explicit_typ)
        || Text.heap_type_eq elemty tabty
           && Text.compare_nullable elemnull tabnull >= 0
      then Ok env
      else
        Error
          (`Type_mismatch
             (Fmt.str "Declared elem of type %a for table of type %a"
                Text.pp_ref_type (elemnull, elemty) Text.pp_ref_type
                (tabnull, tabty) ) )
  in
  let rec check_expr = function
    | [] -> Ok ()
    | { Annotated.raw = Global_get (Text id); _ } :: _
      when not (global_exists id) ->
      Error (`Unknown_global (Text id))
    | { Annotated.raw = Global_get (Text _id); _ } :: _ -> Ok ()
    | { raw = _; _ } :: t -> check_expr t
    (* TODO: complete for other operations *)
  in
  let rec elem_check_init = function
    | [] -> Ok ()
    | { Annotated.raw = l; _ } :: t ->
      let* () = check_expr l in
      elem_check_init t
  in
  let add_memory =
    let seen = Hashtbl.create 512 in
    function
    | None -> Ok ()
    | Some id ->
      if Hashtbl.mem seen id then Error (`Duplicate_memory id)
      else Ok (Hashtbl.add seen id ())
  in
  (* let m = Text.Module.{ m with fields = List.sort Field.compare m.fields } in *)
  (* in some tests, modules are exptected to be evaluated in an order different
    from the one in which they were written. e.g. the last "unknown global" in
    "test/references/global.wast". *)
  let+ (_env : env) =
    let open Module in
    list_fold_left
      (fun env ->
        let open Field in
        function
        | Export _e -> Ok env
        | Func _f -> Ok { env with funcs = true }
        | Start _start ->
          if env.start then Error `Multiple_start_sections
          else Ok { env with start = true }
        | Import i ->
          if env.funcs then Error `Import_after_function
          else if env.declared_memory then Error `Import_after_memory
          else if env.tables then Error `Import_after_table
          else if env.globals then Error `Import_after_global
          else begin
            match i.typ with
            | Mem (id, _) ->
              let* () = add_memory id in
              Ok env
            | Func _ -> Ok env
            | Global (id, _) ->
              let+ () = add_global id in
              env
            | Table (id, ty) ->
              let+ () = add_table id ty in
              env
          end
        | Data _d -> Ok env
        | Elem { typ; mode; explicit_typ; init; _ } ->
          let* env = elem_check_type env typ mode explicit_typ in
          let* () = elem_check_init init in
          Ok env
        | Mem (id, _) ->
          let* () = add_memory id in
          Ok { env with declared_memory = true }
        | Typedef _t -> Ok env
        | Global { id; _ } ->
          let+ () = add_global id in
          { env with globals = true }
        | Table { id; typ; init } ->
          let* () = add_table id typ in
          let* () =
            match init with None -> Ok () | Some e -> check_expr e.raw
          in
          Ok { env with tables = true } )
      (empty_env ()) m.fields
  in

  m
