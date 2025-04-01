# Overview

Given a file `quickstart.wat`, here's how to parse and run this file:

```ocaml
# open Prelude;;
# open Owi;;
# Fmt_tty.setup_std_outputs ();;
- : unit = ()
# Logs.set_level ~all:true (Some Logs.Info);;
- : unit = ()
# Logs.set_reporter (Logs_fmt.reporter ())
- : unit = ()
# let filename = Fpath.v "quickstart.wat";;
val filename : Fpath.t = <abstr>
# let m =
    match Parse.Text.Module.from_file filename with
    | Ok script -> script
    | Error e -> assert false;;
mdx_gen.bc.exe: [INFO] parsing      ...
...
   annots = []}
# let module_to_run, link_state =
    match Compile.Text.until_link Link.empty_state ~unsafe:false ~rac:false ~srac:false ~optimize:false ~name:None m with
    | Ok v -> v
    | Error _ -> assert false;;
mdx_gen.bc.exe: [INFO] checking     ...
mdx_gen.bc.exe: [INFO] typechecking ...
mdx_gen.bc.exe: [INFO] linking      ...
...
# let () =
    match Interpret.Concrete.modul link_state.envs module_to_run with
    | Ok () -> ()
    | Error _ -> assert false;;
mdx_gen.bc.exe: [INFO] interpreting ...
mdx_gen.bc.exe: [INFO] stack         : [  ]
mdx_gen.bc.exe: [INFO] running instr : call 0
mdx_gen.bc.exe: [INFO] calling func  : func f
mdx_gen.bc.exe: [INFO] stack         : [  ]
mdx_gen.bc.exe: [INFO] running instr : i32.const 24
mdx_gen.bc.exe: [INFO] stack         : [ i32.const 24 ]
mdx_gen.bc.exe: [INFO] running instr : i32.const 24
mdx_gen.bc.exe: [INFO] stack         : [ i32.const 24 ; i32.const 24 ]
mdx_gen.bc.exe: [INFO] running instr : i32.add
mdx_gen.bc.exe: [INFO] stack         : [ i32.const 48 ]
mdx_gen.bc.exe: [INFO] running instr : drop
```
