open Owi

(* an extern module that will be linked with a wasm module *)
let extern_module : Concrete_extern_func.extern_func Extern.Module.t =
  (* some custom functions *)
  let memset m start byte length =
    let rec loop offset =
      let b = Concrete_i32.le offset length |> Concrete_boolean.to_bool in
      if b then begin
        match
          Concrete_memory.store_8 m ~addr:(Concrete_i32.add start offset) byte
        with
        | Error _ as e -> e
        | Ok () -> loop (Concrete_i32.add offset (Concrete_i32.of_int 1))
      end
      else Ok ()
    in
    loop Concrete_i32.zero
  in
  let print_x64 (n : Concrete_i64.t) =
    let n = Concrete_i64.to_int64 n in
    Format.printf "0x%LX@\n" n;
    Ok ()
  in
  (* we need to describe their types *)
  let open Concrete_extern_func in
  let open Concrete_extern_func.Syntax in
  let functions =
    [ ("print_x64", Extern_func (i64 ^->. unit, print_x64))
    ; ( "memset"
      , Extern_func (memory 0 ^-> i32 ^-> i32 ^-> i32 ^->. unit, memset) )
    ]
  in
  { Extern.Module.functions; func_type = Concrete_extern_func.extern_type }

(* a link state that contains our custom module, available under the name `chorizo` *)
let link_state =
  Link.State.empty () |> Link.Extern.modul ~name:"chorizo" extern_module

(* a pure wasm module refering to `$extern_mem` *)
let pure_wasm_module =
  match Parse.Text.Module.from_file (Fpath.v "extern_mem.wat") with
  | Error _ -> assert false
  | Ok modul -> modul

(* our pure wasm module, linked with `chorizo` *)
let module_to_run, link_state =
  match
    Compile.Text.until_link link_state ~unsafe:false ~name:None pure_wasm_module
  with
  | Error _ -> assert false
  | Ok v -> v

module I = Interpret.Concrete (Interpret.Default_parameters)

(* let's run it ! it will print the values as defined in the print_i64 function *)
let () =
  match I.modul link_state module_to_run with
  | Error _ -> assert false
  | Ok () -> ()
