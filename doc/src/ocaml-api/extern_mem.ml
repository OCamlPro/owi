open Owi

(* an extern module that will be linked with a wasm module *)
let extern_module : Concrete_extern.Module.t =
  (* some custom functions *)
  let memset m start byte length : _ Concrete_choice.t =
    let rec loop offset : _ Concrete_choice.t =
      let b = Concrete_i32.le offset length |> Concrete_boolean.to_bool in
      if b then
        let to_run =
          Concrete_memory.store_8 m ~addr:(Concrete_i32.add start offset) byte
        in
        begin match Concrete_choice.run to_run Concrete_state.empty with
        | Error e -> Concrete_choice.trap e
        | Ok (_mem, _state) ->
          loop (Concrete_i32.add offset (Concrete_i32.of_int 1))
        end
      else Concrete_choice.return ()
    in
    loop Concrete_i32.zero
  in
  let print_x64 (n : Concrete_i64.t) =
    let n = Concrete_i64.to_int64 n in
    Format.printf "0x%LX@\n" n;
    Concrete_choice.return ()
  in
  (* we need to describe their types *)
  let open Concrete_extern.Func in
  let open Concrete_extern.Func.Syntax in
  [ ("print_x64", Extern_func (i64 ^->. unit, print_x64))
  ; ("memset", Extern_func (memory 0 ^-> i32 ^-> i32 ^-> i32 ^->. unit, memset))
  ]

(* an environment that contains our custom module, available under the name `chorizo` *)
let env =
  let env = Env.Concrete.empty ~context:() in
  Env.Concrete.link_extern_module ~env ~name:"chorizo" extern_module
  |> Stdlib.Result.get_ok

(* a pure wasm module refering to `$extern_mem` *)
let pure_wasm_module =
  match Parse.Text.Module.from_file (Fpath.v "extern_mem.wat") with
  | Error _ -> assert false
  | Ok modul -> modul

(* our pure wasm module, linked with `chorizo` *)
let modul, env =
  match
    Compile.Text.until_concrete_link env ~unsafe:false ~name:None
      pure_wasm_module
  with
  | Error _ -> assert false
  | Ok v -> v

module I = Interpret.Concrete (Interpret.Default_parameters)

(* let's run it ! it will print the values as defined in the print_i64 function *)
let to_run = I.modul ~env ~modul

let () =
  match Concrete_choice.run to_run Concrete_state.empty with
  | Error _ -> assert false
  | Ok (_env, _state) -> ()
