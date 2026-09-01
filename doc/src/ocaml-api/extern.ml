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

(* an environment that contains our custom module, available under the name `sausage` *)
let env =
  let env = Env.Concrete.empty ~context:() in
  Env.Concrete.link_extern_module ~env ~name:"sausage" extern_module
  |> Stdlib.Result.get_ok

(* a pure wasm module refering to `sausage` *)
let pure_wasm_module =
  Parse.Text.Module.from_file (Fpath.v "extern.wat") |> Stdlib.Result.get_ok

(* our pure wasm module, linked with `sausage` *)
let modul, env =
  Compile.Text.until_concrete_link env ~unsafe:false ~name:None pure_wasm_module
  |> Stdlib.Result.get_ok

module I = Interpret.Concrete (Interpret.Default_parameters)

(* let's run it ! it will print the values as defined in the print_i32 function *)
let () =
  match I.modul ~env ~modul with Error _o -> assert false | Ok _env -> ()
