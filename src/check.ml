open Types

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
  let seen_globals = Hashtbl.create 512 in
  let add_global id =
    Option.iter
      (fun id ->
        if Hashtbl.mem seen_globals id then failwith "duplicate global";
        Hashtbl.add seen_globals id () )
      id
  in
  let seen_tables = Hashtbl.create 512 in
  let add_table id =
    Option.iter
      (fun id ->
        if Hashtbl.mem seen_tables id then failwith "duplicate table";
        Hashtbl.add seen_tables id () )
      id
  in
  let seen_memories = Hashtbl.create 512 in
  let add_memory id =
    Option.iter
      (fun id ->
        if Hashtbl.mem seen_memories id then failwith "duplicate memory";
        Hashtbl.add seen_memories id () )
      id
  in
  ignore
  @@ List.fold_left
       (fun env -> function
         | MExport _e -> env
         | MFunc _f -> { env with funcs = true }
         | MStart _start ->
           if env.start then failwith "multiple start sections";
           { env with start = true }
         | MImport i ->
           if env.funcs then failwith "import after function";
           if env.memory then failwith "import after memory";
           if env.tables then failwith "import after table";
           if env.globals then failwith "import after global";
           begin
             match i.desc with
             | Import_mem (id, _) ->
               add_memory id;
               if env.memory || env.imported_memory then
                 failwith "multiple memories";
               { env with imported_memory = true }
             | Import_func _ -> env
             | Import_global (id, _) ->
               add_global id;
               env
             | Import_table (id, _) ->
               add_table id;
               env
           end
         | MData _d -> env
         | MElem _e -> env
         | MMem (id, _) ->
           add_memory id;
           if env.memory || env.imported_memory then
             failwith "multiple memories";
           { env with memory = true }
         | MType _t -> env
         | MGlobal { id; _ } ->
           add_global id;
           { env with globals = true }
         | MTable (id, _) ->
           add_table id;
           { env with tables = true } )
       (empty_env ()) m.fields

let module_ m = try Ok (module_ m) with Failure e -> Error e
