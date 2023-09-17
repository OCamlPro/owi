(** The `spectest` module, to run script from the official test suite. *)

type extern_module = Value.Func.extern_func Link.extern_module

val extern_m : extern_module

(** the spectest module *)
val m : Text.cmd
