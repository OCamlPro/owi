open Owi

(* an extern module that will be linked with a wasm module *)
let extern_module : Link.extern_module =
  (* some custom functions *)
  let rint : int32 ref Value.Extern_ref.ty = Value.Extern_ref.fresh "int ref" in
  let fresh i = ref i in
  let set r (i : int32) = r := i in
  let get r : int32 = !r in
  let print_i32 (i : int32) = Printf.printf "%li\n%!" i in
  (* we need to describe their types *)
  let functions =
    [ ( "print_i32"
      , Value.Func.Extern_func (Func (Arg (I32, Res), R0), print_i32) )
    ; ( "fresh"
      , Value.Func.Extern_func
          (Func (Arg (I32, Res), R1 (Externref rint)), fresh) )
    ; ( "set_i32r"
      , Value.Func.Extern_func
          (Func (Arg (Externref rint, Arg (I32, Res)), R0), set) )
    ; ( "get_i32r"
      , Value.Func.Extern_func (Func (Arg (Externref rint, Res), R1 I32), get)
      )
    ]
  in
  { functions }

(* a link state that contains our custom module, available under the name `sausage` *)
let link_state =
  Link.extern_module Link.empty_state ~name:"sausage" extern_module

(* a pure wasm module refering to `sausage` *)
let pure_wasm_module =
  match Parse.from_file ~filename:"extern.wast" with
  | Error e -> failwith e
  | Ok script -> script

(* our pure wasm module, linked with `sausage` *)
let module_to_run =
  match pure_wasm_module with
  | Types.Symbolic.Module m :: _ -> begin
    match Compile.until_link link_state ~optimize:true ~name:None m with
    | Error msg -> failwith msg
    | Ok (m, _state) -> m
  end
  | _ -> failwith "expected a single module"

(* let's run it ! it will print the values as defined in the print_i32 function *)
let () =
  match Interpret.module_ module_to_run with
  | Error msg -> failwith msg
  | Ok () -> ()
