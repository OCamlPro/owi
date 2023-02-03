open Types
open Syntax

type env =
  { start : bool
  ; memory : bool
  ; imported_memory : bool
  ; funcs : bool
  ; tables : bool
  ; globals : bool
  }

let empty_env () =
  { start = false
  ; memory = false
  ; imported_memory = false
  ; funcs = false
  ; tables = false
  ; globals = false
  }

let module_ m =
  let add_global =
    let seen = Hashtbl.create 512 in
    function
    | None -> Ok ()
    | Some id ->
      if Hashtbl.mem seen id then Error "duplicate global"
      else Ok (Hashtbl.replace seen id ())
  in
  let add_table =
    let seen = Hashtbl.create 512 in
    function
    | None -> Ok ()
    | Some id ->
      if Hashtbl.mem seen id then Error "duplicate table"
      else Ok (Hashtbl.replace seen id ())
  in
  let add_memory =
    let seen = Hashtbl.create 512 in
    function
    | None -> Ok ()
    | Some id ->
      if Hashtbl.mem seen id then Error "duplicate memory"
      else Ok (Hashtbl.add seen id ())
  in

  let* (_env : env) =
    List.fold_left
      (fun env field ->
        let* env = env in
        match field with
        | MExport _e -> Ok env
        | MFunc _f -> Ok { env with funcs = true }
        | MStart _start ->
          if env.start then Error "multiple start sections"
          else Ok { env with start = true }
        | MImport i ->
          if env.funcs then Error "import after function"
          else if env.memory then Error "import after memory"
          else if env.tables then Error "import after table"
          else if env.globals then Error "import after global"
          else begin
            match i.desc with
            | Import_mem (id, _) ->
              let* () = add_memory id in
              if env.memory || env.imported_memory then
                Error "multiple memories"
              else Ok { env with imported_memory = true }
            | Import_func _ -> Ok env
            | Import_global (id, _) ->
              let* () = add_global id in
              Ok env
            | Import_table (id, _) ->
              let* () = add_table id in
              Ok env
          end
        | MData _d -> Ok env
        | MElem _e -> Ok env
        | MMem (id, _) ->
          let* () = add_memory id in
          if env.memory || env.imported_memory then Error "multiple memories"
          else Ok { env with memory = true }
        | MType _t -> Ok env
        | MGlobal { id; _ } ->
          let* () = add_global id in
          Ok { env with globals = true }
        | MTable (id, _) ->
          let* () = add_table id in
          Ok { env with tables = true } )
      (Ok (empty_env ()))
      m.fields
  in

  Ok ()
