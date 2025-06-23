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
  let open Concrete_extern_func.Syntax in
  let functions =
    [ ("print_i32", Concrete_extern_func.Extern_func (i32 ^->. unit, print_i32))
    ; ( "fresh"
      , Concrete_extern_func.Extern_func (i32 ^->. externref rint, fresh) )
    ; ( "set_i32r"
      , Concrete_extern_func.Extern_func (externref rint ^-> i32 ^->. unit, set)
      )
    ; ( "get_i32r"
      , Concrete_extern_func.Extern_func (externref rint ^->. i32, get) )
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
      ~name:None pure_wasm_module
  with
  | Error _ -> assert false
  | Ok v -> v

(* let's run it ! it will print the values as defined in the print_i32 function *)
let () =
  match
    Interpret.Concrete.modul ~timeout:None ~timeout_instr:None link_state.envs
      module_to_run
  with
  | Error _o -> assert false
  | Ok () -> ()
