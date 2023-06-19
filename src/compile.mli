(** Utility functions to compile a module until a given step. *)

val until_check : Symbolic.modul -> Symbolic.modul Result.t

val until_simplify : Symbolic.modul -> Simplified.modul Result.t

(** compile a module with a given link state and produce a new link state and a
    runnable module *)
val until_link :
     Link.state
  -> optimize:bool
  -> name:string option
  -> Symbolic.modul
  -> (Link.module_to_run * Link.state) Result.t

(** compile and interpret a module with a given link state and produce a new
    link state *)
val until_interpret :
     Link.state
  -> optimize:bool
  -> name:string option
  -> Symbolic.modul
  -> Link.state Result.t
