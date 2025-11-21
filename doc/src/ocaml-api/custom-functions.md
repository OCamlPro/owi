# Using and defining external functions (host functions)

## Dealing with the Stack

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
let extern_module : Concrete_extern_func.extern_func Extern.Module.t =
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
  { functions; func_type = Concrete_extern_func.extern_type }

(* a link state that contains our custom module, available under the name `sausage` *)
let link_state =
  Link.State.empty () |> Link.Extern.modul ~name:"sausage" extern_module

(* a pure wasm module refering to `sausage` *)
let pure_wasm_module =
  match Parse.Text.Module.from_file (Fpath.v "extern.wat") with
  | Error _ -> assert false
  | Ok modul -> modul

(* our pure wasm module, linked with `sausage` *)
let module_to_run, link_state =
  match
    Compile.Text.until_link link_state ~unsafe:false ~name:None pure_wasm_module
  with
  | Error _ -> assert false
  | Ok v -> v

(* let's run it ! it will print the values as defined in the print_i32 function *)
let () =
  match
    Interpret.Concrete.modul ~timeout:None ~timeout_instr:None link_state
      module_to_run
  with
  | Error _o -> assert false
  | Ok () -> ()
```

You'll get the expected result:

```sh
$ ./extern.exe
42
13
```

## Dealing with the Linear Memory

Owi also allows interacting with linear memory through external functions.
This is helpful because it enables the host system to communicate directly
with a Wasm instance through its linear memory. Consider the tiny example
below to illustrate this:

<!-- $MDX file=extern_mem.wat -->
```wat
(module $extern_mem

  (import "chorizo" "memset" (func $memset (param i32 i32 i32)))

  (import "chorizo" "print_x64" (func $print_x64 (param i64)))

  (memory 1)

  (func $start

    ;; memset 0 0xAA 8
    (call $memset (i32.const 0) (i32.const 0xAA) (i32.const 8))

    ;; print_x64 (load 0)
    (call $print_x64 (i64.load (i32.const 0)))
  )

  (start $start)
)
```

In the module `$extern_mem`, we first import `$memset` and `$print_x64`. Then,
in the `$start` function, we initialize the memory starting at address
`(i32.const 0)` with a sequence of length `(i32.const 8)` with bytes of
`(i32.const 0xAA)`.

The definition of the external functions follows the same format as the
[previous example]. The difference is that, now, in the GADT definition of
memset to allow the memory to be passed to this function, we need to wrap
the three I32 arguments in a Mem variant. That is, instead of writing
memset as:

<!-- $MDX skip -->
```ocaml
(Func (Arg (I32, (Arg (I32, (Arg (I32, Res))))), R0), memset)
```

One should use:

<!-- $MDX skip -->
```ocaml
(Func (Mem (Arg (I32, (Arg (I32, (Arg (I32, Res)))))), R0), memset)
```

See the module below for the whole implementation:

<!-- $MDX file=extern_mem.ml -->
```ocaml
open Owi

(* an extern module that will be linked with a wasm module *)
let extern_module : Concrete_extern_func.extern_func Extern.Module.t =
  (* some custom functions *)
  let memset m start byte length =
    let rec loop offset =
      if offset <= length then begin
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

(* let's run it ! it will print the values as defined in the print_i64 function *)
let () =
  match
    Interpret.Concrete.modul ~timeout:None ~timeout_instr:None link_state
      module_to_run
  with
  | Error _ -> assert false
  | Ok () -> ()
```

Running the above program should yield:

```sh
$ ./extern_mem.exe
0xAAAAAAAAAAAAAAAA
```

## Advanced Usage

To learn more, see our advanced [Game of Life] example
based on the famous cellular automaton by Conway. It show how to link several modules from different `.wat` files.

[Game of Life]: ./life_game
[previous example]: ./README.md#dealing-with-the-stack
