open Types

let module_ m =
  let start = ref false in
  let memory = ref false in

  let funcs = ref false in
  let tables = ref false in
  let globals = ref false in

  List.iter
    (function
      | MExport _e -> ()
      | MFunc _f -> funcs := true
      | MStart _start ->
        if !start then
          failwith "multiple start section"
        else
          start := true
      | MImport _i ->
        if !funcs then failwith "import after function definition";
        if !memory then failwith "import after memory definition";
        if !tables then failwith "import after table definition";
        if !globals then failwith "import after globals definition"
      | MData _d -> ()
      | MElem _e -> ()
      | MMem _m ->
        if !memory then failwith "multiple memories are not allowed (yet)"
      | MType _t -> ()
      | MGlobal _g -> globals := true
      | MTable _t -> tables := true )
    m.fields

let script s =
  List.iter
    (function
      | Module m -> module_ m
      | _ -> () )
    s
