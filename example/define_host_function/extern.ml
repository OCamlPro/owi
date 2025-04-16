open Owi

(* an extern module that will be linked with a wasm module *)
let extern_module : Concrete_extern_func.extern_func Link.extern_module =
  (* some custom functions *)
  let rint : int32 ref Type.Id.t = Type.Id.make () in
  let fresh i = Ok (ref i) in
  let set r (i : int32) =
    r := i;
    Ok ()
  in
  let get r = Ok !r in
  let print_i32 (i : int32) =
    Printf.printf "%li\n%!" i;
    Ok ()
  in
  (* we need to describe their types *)
  let functions =
    [ ( "print_i32"
      , Concrete_extern_func.Extern_func (Func (Arg (I32, Res), R0), print_i32)
      )
    ; ( "fresh"
      , Concrete_extern_func.Extern_func
          (Func (Arg (I32, Res), R1 (Externref rint)), fresh) )
    ; ( "set_i32r"
      , Concrete_extern_func.Extern_func
          (Func (Arg (Externref rint, Arg (I32, Res)), R0), set) )
    ; ( "get_i32r"
      , Concrete_extern_func.Extern_func
          (Func (Arg (Externref rint, Res), R1 I32), get) )
    ]
  in
  { functions }

(* a link state that contains our custom module, available under the name `sausage` *)
let link_state =
  Link.extern_module Link.empty_state ~name:"sausage" extern_module

(* a pure wasm module refering to `sausage` *)
let pure_wasm_module =
  match Parse.Text.Module.from_file (Fpath.v "extern.wat") with
  | Error _ -> assert false
  | Ok modul -> modul

(* our pure wasm module, linked with `sausage` *)
let module_to_run, link_state =
  match
    Compile.Text.until_link link_state ~unsafe:false ~rac:false ~srac:false
      ~optimize:true ~name:None pure_wasm_module
  with
  | Error _ -> assert false
  | Ok v -> v

(* let's run it ! it will print the values as defined in the print_i32 function *)
let () =
  match Interpret.Concrete.modul link_state.envs module_to_run with
  | Error _ -> assert false
  | Ok () -> ()
