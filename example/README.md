# Examples

## Using the `spectest` module

Given the following `print.wast` file:

<!-- $MDX file=print.wast -->
```wast
(module
  (func $print_i32 (import "spectest" "print_i32") (param i32))
  (func $main (call $print_i32 (i32.const 42)))
  (start $main)
)
```

You can print the value thanks to the `print_i32` function imported from the `spectest` module:

```sh
$ dune exec ../src/bin/owi.exe -- ./print.wast --script
42
```

## Using and defining external functions (host functions)

Given the following `extern.wast` file:

<!-- $MDX file=extern.wast -->
```wast
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
```

You'll get the expected result:

```sh
$ dune exec -- ./extern.exe
42
13
```
