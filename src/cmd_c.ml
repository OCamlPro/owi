open Bos

let ( let* ) = Result.bind

let list_map f lst =
  let exception E of Rresult.R.msg in
  try
    Ok
      (List.map
         (fun x -> match f x with Ok x' -> x' | Error e -> raise (E e))
         lst )
  with E e -> Error e

type deps =
  { clang : flags:Cmd.t -> out:Fpath.t -> Fpath.t -> Cmd.t
  ; opt : Fpath.t -> Cmd.t
  ; llc : bc:Fpath.t -> obj:Fpath.t -> Cmd.t
  ; ld : flags:Cmd.t -> out:Fpath.t -> Fpath.t list -> Cmd.t
  ; wasm2wat : out:Fpath.t -> Fpath.t -> Cmd.t
  }

type metadata =
  { arch : int
  ; property : string option
  ; files : string list
  }

let clang bin ~flags ~out file = Cmd.(bin %% flags % "-o" % p out % p file)

let opt bin file = Cmd.(bin % "-O1" % "-o" % p file % p file)

let llc bin ~bc ~obj =
  let flags = Cmd.of_list [ "-O1"; "-march=wasm32"; "-filetype=obj"; "-o" ] in
  Cmd.(bin %% flags % p obj % p bc)

let ld bin ~flags ~out files =
  let libc = C_share.get_libc () |> Option.get in
  let files = List.fold_left (fun acc f -> Cmd.(acc % p f)) Cmd.empty files in
  Cmd.(bin %% flags % "-o" % p out %% files % libc)

let wasm2wat bin0 ~out bin = Cmd.(bin0 % "-o" % p out % p bin)

let check_dependencies () =
  let* clang_bin = OS.Cmd.resolve @@ Cmd.v "clang" in
  let* opt_bin = OS.Cmd.resolve @@ Cmd.v "opt" in
  let* llc_bin = OS.Cmd.resolve @@ Cmd.v "llc" in
  let* ld_bin = OS.Cmd.resolve @@ Cmd.v "wasm-ld" in
  let* wasm2wat_bin = OS.Cmd.resolve @@ Cmd.v "wasm2wat" in
  Ok
    { clang = clang clang_bin
    ; opt = opt opt_bin
    ; llc = llc llc_bin
    ; ld = ld ld_bin
    ; wasm2wat = wasm2wat wasm2wat_bin
    }

let pre_patterns : (Re2.t * string) array =
  Array.map
    (fun (regex, template) -> (Re2.create_exn regex, template))
    [| ( "void\\s+reach_error\\(\\)\\s*\\{.*\\}"
       , "void reach_error() { owi_assert(0); }" )
       (* ugly: Hack to solve duplicate errors on compilation *)
       (* ; ("void\\s+(assert|assume)\\(", "void old_\\1(") *)
    |]

let patch_with_regex ~patterns (data : string) : string =
  Array.fold_left
    (fun data (regex, template) -> Re2.rewrite_exn regex ~template data)
    data patterns

let patch ~(src : Fpath.t) ~(dst : Fpath.t) =
  let* data = OS.File.read src in
  let data = patch_with_regex ~patterns:pre_patterns data in
  let data =
    String.concat "\n"
      [ "#define __attribute__(x)"
      ; "#define __extension__"
      ; "#define __restrict"
      ; "#define __inline"
      ; "#include <owi.h>"
      ; data
      ]
  in
  let* () = OS.File.write dst data in
  Ok ()

let copy ~src ~dst =
  let* data = OS.File.read src in
  let* () = OS.File.write dst data in
  Ok dst

let instrument_file ?(skip = false) ~(includes : string list)
  ~(workspace : Fpath.t) (file : string) =
  let file = Fpath.v file in
  let dst = Fpath.(workspace // base (file -+ ".c")) in
  if skip then copy ~src:file ~dst
  else begin
    Logs.app (fun m -> m "instrumenting %a" Fpath.pp file);
    let* () = patch ~src:file ~dst in
    let pypath = String.concat ":" C_share.py_location in
    let* () = OS.Env.set_var "PYTHONPATH" (Some pypath) in
    begin
      try
        Py.initialize ();
        C_instrumentor.instrument (Fpath.to_string dst) includes;
        Py.finalize ()
      with Py.E (errtype, errvalue) ->
        let pp = Py.Object.format in
        Logs.warn (fun m -> m "instrumentor: %a: %a" pp errtype pp errvalue)
    end;
    Ok dst
  end

let compile ~(deps : deps) ~(includes : string list) ~(opt_lvl : string)
  (file : Fpath.t) =
  Logs.app (fun m -> m "compiling %a" Fpath.pp file);
  let cflags =
    let includes = Cmd.of_list ~slip:"-I" includes in
    let warnings =
      Cmd.of_list
        [ "-Wno-int-conversion"
        ; "-Wno-pointer-sign"
        ; "-Wno-string-plus-int"
        ; "-Wno-implicit-function-declaration"
        ; "-Wno-incompatible-library-redeclaration"
        ; "-Wno-incompatible-function-pointer-types"
        ; "-Wno-incompatible-pointer-types"
        ]
    in
    Cmd.(
      of_list
        [ "-O" ^ opt_lvl; "-g"; "-emit-llvm"; "--target=wasm32"; "-m32"; "-c" ]
      %% warnings %% includes )
  in
  let bc = Fpath.(file -+ ".bc") in
  let obj = Fpath.(file -+ ".o") in
  let* () = OS.Cmd.run @@ deps.clang ~flags:cflags ~out:bc file in
  let* () = OS.Cmd.run @@ deps.opt bc in
  let* () = OS.Cmd.run @@ deps.llc ~bc ~obj in
  Ok obj

let link ~deps ~workspace (files : Fpath.t list) =
  let ldflags ~entry =
    let stack_size = 8 * (1024 * 1024) in
    Cmd.(
      of_list
        [ "-z"; "stack-size=" ^ string_of_int stack_size; "--export=" ^ entry ] )
  in
  let wasm = Fpath.(workspace / "a.out.wasm") in
  let wat = Fpath.(workspace / "a.out.wat") in
  let* () =
    OS.Cmd.run @@ deps.ld ~flags:(ldflags ~entry:"_start") ~out:wasm files
  in
  let* () = OS.Cmd.run @@ deps.wasm2wat ~out:wat wasm in
  Ok wat

let cleanup dir =
  OS.Path.fold ~elements:`Files
    (fun path _acc ->
      if not @@ Fpath.has_ext ".wat" path then
        match OS.Path.delete path with
        | Ok () -> ()
        | Error (`Msg e) -> Logs.warn (fun m -> m "%s" e) )
    () [ dir ]
  |> Logs.on_error_msg ~level:Logs.Warning ~use:Fun.id

let owi ~workspace ~workers flags module_ =
  let flags = Cmd.of_list flags in
  let w = string_of_int workers in
  Cmd.(
    v "owi" % "sym" % "-w" % w % "--workspace" %% flags % p workspace
    % p module_ )

let run ~workspace ~workers module_ =
  Logs.app (fun m -> m "running %a" Fpath.pp module_);
  let workspace = Fpath.(workspace / "test-suite") in
  let* () = OS.Cmd.run @@ owi ~workspace ~workers [] module_ in
  Ok 0

let pp_tm fmt Unix.{ tm_year; tm_mon; tm_mday; tm_hour; tm_min; tm_sec; _ } =
  Format.pp fmt "%04d-%02d-%02dT%02d:%02d:%02dZ" (tm_year + 1900) tm_mon tm_mday
    tm_hour tm_min tm_sec

let metadata ~workspace arch property files =
  let out_metadata chan { arch; property; files } =
    let o = Xmlm.make_output ~nl:true ~indent:(Some 2) (`Channel chan) in
    let tag n = (("", n), []) in
    let el n d = `El (tag n, [ `Data d ]) in
    let* spec =
      match property with None -> Ok "" | Some f -> OS.File.read @@ Fpath.v f
    in
    let file = String.concat " " files in
    let hash = List.fold_left (fun _ f -> Sha256.file f) Sha256.zero files in
    let time = Unix.time () |> Unix.localtime in
    let test_metadata =
      `El
        ( tag "test-metadata"
        , [ el "sourcecodelang" "C"
          ; el "producer" "owic"
          ; el "specification" (String.trim spec)
          ; el "programfile" file
          ; el "programhash" (Sha256.to_hex hash)
          ; el "entryfunction" "main"
          ; el "architecture" (Format.sprintf "%dbit" arch)
          ; el "creationtime" (Format.asprintf "%a" pp_tm time)
          ] )
    in
    let dtd =
      "<!DOCTYPE test-metadata PUBLIC \"+//IDN sosy-lab.org//DTD test-format \
       test-metadata 1.1//EN\" \
       \"https://sosy-lab.org/test-format/test-metadata-1.1.dtd\">"
    in
    Xmlm.output o (`Dtd (Some dtd));
    Xmlm.output_tree Fun.id o test_metadata;
    Ok ()
  in
  let fpath = Fpath.(workspace / "test-suite" / "metadata.xml") in
  let* _ = OS.Dir.create ~path:true (Fpath.parent fpath) in
  let* res = OS.File.with_oc fpath out_metadata { arch; property; files } in
  res

let cmd debug arch property testcomp output workers opt_lvl includes files =
  if debug then Logs.set_level (Some Debug);
  let workspace = Fpath.v output in
  let includes = C_share.lib_location @ includes in
  let res =
    let* deps = check_dependencies () in
    let* _ = OS.Dir.create ~path:true workspace in
    (* skip instrumentation if not in test-comp mode *)
    let skip = not testcomp in
    let* nfiles = list_map (instrument_file ~skip ~includes ~workspace) files in
    let* objects = list_map (compile ~deps ~includes ~opt_lvl) nfiles in
    let* module_ = link ~deps ~workspace objects in
    cleanup workspace;
    let* () = metadata ~workspace arch property files in
    run ~workspace ~workers module_
  in
  match Logs.on_error_msg ~level:Logs.Error ~use:(fun _ -> 1) res with
  | 0 -> ()
  | n -> exit n
