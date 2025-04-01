open Graphics
open Owi

let () = Random.self_init ()

(* an extern module that will be linked with a wasm module *)
let extern_module : Concrete_extern_func.extern_func Link.extern_module =
  (* some custom functions *)
  let x = ref 0 in
  let y = ref 0 in
  let height (_ : int32) = Ok 200l in
  let width (_ : int32) = Ok 300l in
  let sleep (_ : int32) =
    Unix.sleepf 0.2;
    Ok ()
  in
  let newline (_ : int32) =
    x := 0;
    incr y;
    Ok ()
  in
  let cell_print (i : int32) =
    set_color (if i = 1l then black else white);
    fill_rect (!x * 2) (!y * 2) 2 2;
    incr x;
    Ok ()
  in
  let clear_screen (_ : int32) =
    synchronize ();
    x := 0;
    y := 0;
    clear_graph ();
    Ok ()
  in
  let rand_val (bound : int32) =
    let r = Random.int32 bound in
    Ok (if r = 0l then 1l else 0l)
  in
  let init_window (_ : int32) =
    open_graph " 600x400";
    auto_synchronize false;
    clear_graph ();
    Ok ()
  in
  (* we need to describe their types *)
  let functions =
    [ ( "height"
      , Concrete_extern_func.Extern_func (Func (Arg (I32, Res), R1 I32), height)
      )
    ; ( "width"
      , Concrete_extern_func.Extern_func (Func (Arg (I32, Res), R1 I32), width)
      )
    ; ( "sleep"
      , Concrete_extern_func.Extern_func (Func (Arg (I32, Res), R0), sleep) )
    ; ( "newline"
      , Concrete_extern_func.Extern_func (Func (Arg (I32, Res), R0), newline) )
    ; ( "cell_print"
      , Concrete_extern_func.Extern_func (Func (Arg (I32, Res), R0), cell_print)
      )
    ; ( "clear_screen"
      , Concrete_extern_func.Extern_func
          (Func (Arg (I32, Res), R0), clear_screen) )
    ; ( "rand_val"
      , Concrete_extern_func.Extern_func
          (Func (Arg (I32, Res), R1 I32), rand_val) )
    ; ( "init_window"
      , Concrete_extern_func.Extern_func (Func (Arg (I32, Res), R0), init_window)
      )
    ]
  in
  { functions }

(* a link state that contains our custom module, available under the name `life_ext` *)
let link_state =
  Link.extern_module Link.empty_state ~name:"life_ext" extern_module

(* first pure wasm module refering to `life_ext` *)
let pure_wasm_module_1 =
  match Parse.Text.Module.from_file (Fpath.v "life.wat") with
  | Error _ -> assert false
  | Ok modul -> modul

(* our first pure wasm module, linked with `life_ext` *)
let module_to_run, link_state =
  match
    Compile.Text.until_link link_state ~unsafe:false ~rac:false ~srac:false
      ~optimize:true ~name:(Some "life") pure_wasm_module_1
  with
  | Error _ -> assert false
  | Ok (m, state) -> (m, state)

(* let's run it ! First module to be interpreted *)
let () =
  match Interpret.Concrete.modul link_state.envs module_to_run with
  | Error _ -> assert false
  | Ok () -> ()

(* second pure wasm module refering to `life_ext` *)
let pure_wasm_module_2 =
  match Parse.Text.Module.from_file (Fpath.v "life_loop.wat") with
  | Error _ -> assert false
  | Ok modul -> modul

(* our second pure wasm module, linked with `life_ext` and `life` interpreted before *)
let module_to_run, link_state =
  match
    Compile.Text.until_link link_state ~unsafe:false ~rac:false ~srac:false
      ~optimize:true ~name:None pure_wasm_module_2
  with
  | Error _ -> assert false
  | Ok (m, state) -> (m, state)

(* let's run it ! it will animate the game of life in a graphics window *)
let () =
  match Interpret.Concrete.modul link_state.envs module_to_run with
  | Error _ -> assert false
  | Ok () -> ()
