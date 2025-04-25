# Using the library

## Quickstart

Given a file `quickstart.wat`, here's how to parse and run this file:

```ocaml
# open Prelude;;
# open Owi;;
# Logs.set_level (Some Logs.Info);;
- : unit = ()
# let filename = Fpath.v "quickstart.wat";;
val filename : Fpath.t = <abstr>
# let m =
    match Parse.Text.Module.from_file filename with
    | Ok script -> script
    | Error e -> assert false;;
val m : Text.modul =
...
# let module_to_run, link_state =
    match Compile.Text.until_link Link.empty_state ~unsafe:false ~rac:false ~srac:false ~optimize:false ~name:None m with
    | Ok v -> v
    | Error _ -> assert false;;
val module_to_run : '_weak1 Link.module_to_run =
...
val link_state : '_weak1 Link.state =
...
# let () =
    match Interpret.Concrete.modul link_state.envs module_to_run with
    | Ok () -> ()
    | Error _ -> assert false;;
```
