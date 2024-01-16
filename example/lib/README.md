# Using the library

## Quickstart

Given a file `quickstart.wat`, here's how to parse and run this file:

```ocaml
# open Owi;;
# let filename = "quickstart.wat";;
val filename : string = "quickstart.wat"
# let m =
    match Parse.Module.from_file ~filename with
    | Ok script -> script
    | Error e -> failwith e;;
val m : Text.modul =
...
# let module_to_run, link_state =
    match Compile.until_link Link.empty_state ~unsafe:false ~optimize:false ~name:None m with
    | Ok v -> v
    | Error e -> failwith e;;
val module_to_run : '_weak1 Link.module_to_run =
...
val link_state : '_weak1 Link.state =
...
# let () =
    Log.debug_on := true;
    match Interpret.Concrete.modul link_state.envs module_to_run with
    | Ok () -> ()
    | Error e -> failwith e;;
interpreting ...
stack        : [  ]
running instr: call 0
calling func : func f
stack        : [  ]
running instr: i32.const 24
stack        : [ i32.const 24 ]
running instr: i32.const 24
stack        : [ i32.const 24 ; i32.const 24 ]
running instr: i32.add
stack        : [ i32.const 48 ]
running instr: drop
stack        : [  ]
stack        : [  ]
```
