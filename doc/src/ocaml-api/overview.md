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
# let module_to_run, link_state =
    match Compile.Text.until_link (Link.State.empty ()) ~unsafe:false ~name:None m with
    | Ok v -> v
    | Error _ -> assert false;;
mdx_gen.bc.exe: [INFO] checking     ...
mdx_gen.bc.exe: [INFO] typechecking ...
...
mdx_gen.bc.exe: [INFO] linking      ...
...
# module I = Interpret.Concrete (Interpret.Default_parameters);;
module I :
  sig
    val modul :
      Concrete_extern_func.extern_func Link.State.t ->
      Concrete_extern_func.extern_func Linked.Module.t -> unit Owi.Result.t
  end
# let () =
    match I.modul link_state module_to_run with
    | Ok () -> ()
    | Error _ -> assert false;;
mdx_gen.bc.exe: [INFO] interpreting ...
mdx_gen.bc.exe: [INFO] stack         : [  ]
mdx_gen.bc.exe: [INFO] running instr : call 0 (executed 0 times)
mdx_gen.bc.exe: [INFO] calling func  : func f
mdx_gen.bc.exe: [INFO] stack         : [  ]
mdx_gen.bc.exe: [INFO] running instr : i32.const 24 (executed 0 times)
mdx_gen.bc.exe: [INFO] stack         : [ i32.const 24 ]
mdx_gen.bc.exe: [INFO] running instr : i32.const 24 (executed 0 times)
mdx_gen.bc.exe: [INFO] stack         : [ i32.const 24 ; i32.const 24 ]
mdx_gen.bc.exe: [INFO] running instr : i32.add (executed 0 times)
mdx_gen.bc.exe: [INFO] stack         : [ i32.const 48 ]
mdx_gen.bc.exe: [INFO] running instr : drop (executed 0 times)
```
