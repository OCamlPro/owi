(** Utility functions to compile a module until a given step. *)

(** compile a module with a given link state and produce a new link state and a
    runnable module *)
val until_link :
     Link.state
  -> Types.Symbolic.modul
  -> (Link.module_to_run * Link.state, string) result

(** compile and interpret a module with a given link state and produce a new
    link state *)
val until_interpret : Link.state -> Types.Symbolic.modul -> (Link.state, string) result
