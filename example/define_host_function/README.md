# Using and defining external functions (host functions)

Given the following `extern.wat` file:

<!-- $MDX file=extern.wat -->
```wat
(module $extern

  (import "sausage" "fresh"
    (func $fresh (param i32) (result externref)))

  (import "sausage" "get_i32r"
    (func $get (param externref) (result i32)))

  (import "sausage" "set_i32r"
    (func $set (param externref) (param i32)))

  (import "sausage" "print_i32"
    (func $print_i32 (param i32)))

  (func $start (local $ref externref)

    ;; let ref = fresh 42
    (local.set $ref (call $fresh (i32.const 42)))

    ;; print_i32 (get ref)
    (call $print_i32 (call $get (local.get $ref)))

    ;; set ref 13
    (call $set (local.get $ref) (i32.const 13)  )

    ;; print_i32 (get ref)
    (call $print_i32 (call $get (local.get $ref)))

  )

  (start $start)
)
```

You can define the various required external functions in OCaml like this :

<!-- $MDX file=extern.ml -->
```ocaml
open Owi

(* an extern module that will be linked with a wasm module *)
let extern_module : Concrete_value.Func.extern_func Link.extern_module =
  (* some custom functions *)
  let rint : int32 ref Type.Id.t = Type.Id.make () in
  let fresh i = ref i in
  let set r (i : int32) = r := i in
  let get r : int32 = !r in
  let print_i32 (i : int32) = Printf.printf "%li\n%!" i in
  (* we need to describe their types *)
  let functions =
    [ ( "print_i32"
      , Concrete_value.Func.Extern_func (Func (Arg (I32, Res), R0), print_i32)
      )
    ; ( "fresh"
      , Concrete_value.Func.Extern_func
          (Func (Arg (I32, Res), R1 (Externref rint)), fresh) )
    ; ( "set_i32r"
      , Concrete_value.Func.Extern_func
          (Func (Arg (Externref rint, Arg (I32, Res)), R0), set) )
    ; ( "get_i32r"
      , Concrete_value.Func.Extern_func
          (Func (Arg (Externref rint, Res), R1 I32), get) )
    ]
  in
  { functions }

(* a link state that contains our custom module, available under the name `sausage` *)
let link_state =
  Link.extern_module Link.empty_state ~name:"sausage" extern_module

(* a pure wasm module refering to `sausage` *)
let pure_wasm_module =
  match Parse.Module.from_file (Fpath.v "extern.wat") with
  | Error e -> failwith e
  | Ok modul -> modul

(* our pure wasm module, linked with `sausage` *)
let module_to_run, link_state =
  match
    Compile.until_link link_state ~unsafe:false ~optimize:true ~name:None
      pure_wasm_module
  with
  | Error msg -> failwith msg
  | Ok v -> v

(* let's run it ! it will print the values as defined in the print_i32 function *)
let () =
  match Interpret.Concrete.modul link_state.envs module_to_run with
  | Error msg -> failwith msg
  | Ok () -> ()
```

You'll get the expected result:

```sh
$ dune exec -- ./extern.exe
42
13
```

To learn more, see our advanced [Game of Life] example
based on the famous cellular automaton by Conway. It show how to link several modules from different `.wat` files.

[Game of Life]: ./life_game
