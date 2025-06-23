open Owi

(* an extern module that will be linked with a wasm module *)
let extern_module : Concrete_extern_func.extern_func Link.extern_module =
  (* some custom functions *)
  let memset m start byte length =
    let rec loop offset =
      if Int32.le offset length then begin
        match Concrete_memory.store_8 m ~addr:(Int32.add start offset) byte with
        | Error _ as e -> e
        | Ok () -> loop (Int32.add offset 1l)
      end
      else Ok ()
    in
    loop 0l
  in
  let print_x64 (i : int64) =
    Printf.printf "0x%LX\n%!" i;
    Ok ()
  in
  (* we need to describe their types *)
  let open Concrete_extern_func in
  let open Concrete_extern_func.Syntax in
  let functions =
    [ ("print_x64", Extern_func (i64 ^->. unit, print_x64))
    ; ("memset", Extern_func (memory ^-> i32 ^-> i32 ^-> i32 ^->. unit, memset))
    ]
  in
  { functions }

(* a link state that contains our custom module, available under the name `chorizo` *)
let link_state =
  Link.extern_module Link.empty_state ~name:"chorizo" extern_module

(* a pure wasm module refering to `$extern_mem` *)
let pure_wasm_module =
  match Parse.Text.Module.from_file (Fpath.v "extern_mem.wat") with
  | Error _ -> assert false
  | Ok modul -> modul

(* our pure wasm module, linked with `chorizo` *)
let module_to_run, link_state =
  match
    Compile.Text.until_link link_state ~unsafe:false ~rac:false ~srac:false
      ~name:None pure_wasm_module
  with
  | Error _ -> assert false
  | Ok v -> v

(* let's run it ! it will print the values as defined in the print_i64 function *)
let () =
  match
    Interpret.Concrete.modul ~timeout:None ~timeout_instr:None link_state.envs
      module_to_run
  with
  | Error _ -> assert false
  | Ok () -> ()
