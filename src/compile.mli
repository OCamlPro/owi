(** Utility functions to compile a module until a given step. *)

val until_check : ?unsafe:bool -> Text.modul -> Text.modul Result.t

val until_simplify : ?unsafe:bool -> Text.modul -> Simplified.modul Result.t

(** compile a module with a given link state and produce a new link state and a
    runnable module *)
val until_link :
     ?unsafe:bool
  -> 'f Link.state
  -> optimize:bool
  -> name:string option
  -> Text.modul
  -> ('f Link.module_to_run * 'f Link.state) Result.t

(** compile and interpret a module with a given link state and produce a new
    link state *)
val until_interpret :
     Concrete_value.Func.extern_func Link.state
  -> optimize:bool
  -> name:string option
  -> Text.modul
  -> Concrete_value.Func.extern_func Link.state Result.t
