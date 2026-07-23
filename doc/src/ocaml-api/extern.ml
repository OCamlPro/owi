open Owi

(* an extern module that will be linked with a wasm module *)
let extern_module : Concrete_extern.Module.t =
  (* some custom functions *)
  let rint : Concrete_i32.t ref Type.Id.t = Type.Id.make () in
  let fresh i = Ok (ref i) in
  let set r (i : Concrete_i32.t) =
    r := i;
    Ok ()
  in
  let get r = Ok !r in
  let print_i32 (i : Concrete_i32.t) =
    Format.printf "%a\n%!" Concrete_i32.pp i;
    Ok ()
  in
  (* we need to describe their types *)
  let open Concrete_extern.Func in
  let open Concrete_extern.Func.Syntax in
  [ ("print_i32", Extern_func (i32 ^->. unit, print_i32))
  ; ("fresh", Extern_func (i32 ^->. externref rint, fresh))
  ; ("set_i32r", Extern_func (externref rint ^-> i32 ^->. unit, set))
  ; ("get_i32r", Extern_func (externref rint ^->. i32, get))
  ]

(* a link state that contains our custom module, available under the name `sausage` *)
let link_state =
  Link.State.empty ()
  |> Link.Extern.concrete_module ~name:"sausage" extern_module

(* a pure wasm module refering to `sausage` *)
let pure_wasm_module =
  match Parse.Text.Module.from_file (Fpath.v "extern.wat") with
  | Error _ -> assert false
  | Ok modul -> modul

(* our pure wasm module, linked with `sausage` *)
let module_to_run, link_state =
  match
    Compile.Text.until_concrete_link link_state ~unsafe:false ~name:None
      pure_wasm_module
  with
  | Error _ -> assert false
  | Ok v -> v

module I = Interpret.Concrete (Interpret.Default_parameters)

(* let's run it ! it will print the values as defined in the print_i32 function *)
let () =
  match I.modul link_state module_to_run with
  | Error _o -> assert false
  | Ok () -> ()
