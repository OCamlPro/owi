open Owi

let link_state = Link.empty_state

(* TODO: put this in examples

   let extern_module : Link.extern_module =
     let module M = struct
       let rint : int32 ref Value.Extern_ref.ty = Value.Extern_ref.fresh "int ref"

       let fresh i = ref i

       let set r (i : int32) = r := i

       let get r : int32 = !r
     end in
     let print_i32 (i : Int32.t) = Printf.printf "%li\n%!" i in
     let functions =
       [ ( "print_i32"
         , Value.Func.Extern_func (Func (Arg (I32, Res), R0), print_i32) )
       ; ( "fresh"
         , Value.Func.Extern_func
             (Func (Arg (I32, Res), R1 (Externref M.rint)), M.fresh) )
       ; ( "set_i32r"
         , Value.Func.Extern_func
             (Func (Arg (Externref M.rint, Arg (I32, Res)), R0), M.set) )
       ; ( "get_i32r"
         , Value.Func.Extern_func
             (Func (Arg (Externref M.rint, Res), R1 I32), M.get) )
       ]
     in
     { functions }

   let link_state = Link.extern_module "stuff" extern_module Link.empty_state in
*)

let simplify_then_link_then_run file =
  let to_run, _link_state =
    List.fold_left
      (fun ((to_run, state) as acc) -> function
        | Types.Module m -> begin
          match Compile.until_link state m with
          | Ok (m, state) -> (m :: to_run, state)
          | Error msg -> failwith msg
        end
        | Types.Register (name, id) ->
          (to_run, Link.register_module state ~name ~id)
        | _ -> acc )
      ([], link_state) file
  in
  List.iter
    (fun m ->
      let res = Interpret.module_ m in
      Result.fold ~ok:Fun.id ~error:failwith res )
    (List.rev to_run)

let exec, files =
  let exec = ref simplify_then_link_then_run in
  let files = ref [] in
  let spec =
    Arg.
      [ ( "--script"
        , Unit (fun () -> exec := Script.exec)
        , "run as a reference test suite script" )
      ; ("-s", Unit (fun () -> exec := Script.exec), "short for --script")
      ; ("--debug", Set Log.debug_on, "debug mode")
      ; ("-d", Set Log.debug_on, "short for --debug")
      ]
  in
  Arg.parse spec (fun s -> files := s :: !files) "wast interpreter %s <file>";
  (!exec, !files)

let run_file filename =
  if not @@ Sys.file_exists filename then
    Log.err "file `%s` doesn't exist" filename;
  match Parse.from_file ~filename with
  | Ok script -> exec script
  | Error e -> Log.err "%s" e

let () = List.iter run_file files
