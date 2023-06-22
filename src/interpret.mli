(** Module to interpret a linked module. *)

(** interpret a module *)
val modul : Link.module_to_run -> unit Result.t

(** interpret a function with a given input stack and produce a new stack*)
val exec_vfunc :
     Link.Env.t' Stack.t
  -> (Link.Env.t', Value.Func.extern_func) Value.Func.t
  -> Link.Env.t' Stack.t Result.t

val exec_iunop :
  Link.Env.t' Stack.t -> Types.nn -> Types.iunop -> Link.Env.t' Stack.t

val exec_funop :
  Link.Env.t' Stack.t -> Types.nn -> Types.funop -> Link.Env.t' Stack.t

val exec_ibinop :
  Link.Env.t' Stack.t -> Types.nn -> Types.ibinop -> Link.Env.t' Stack.t

val exec_fbinop :
  Link.Env.t' Stack.t -> Types.nn -> Types.fbinop -> Link.Env.t' Stack.t

val exec_itestop :
  Link.Env.t' Stack.t -> Types.nn -> Types.itestop -> Link.Env.t' Stack.t

val exec_irelop :
  Link.Env.t' Stack.t -> Types.nn -> Types.irelop -> Link.Env.t' Stack.t

val exec_frelop :
  Link.Env.t' Stack.t -> Types.nn -> Types.frelop -> Link.Env.t' Stack.t

val exec_itruncf :
  Link.Env.t' Stack.t -> Types.nn -> Types.nn -> Types.sx -> Link.Env.t' Stack.t

val exec_itruncsatf :
  Link.Env.t' Stack.t -> Types.nn -> Types.nn -> Types.sx -> Link.Env.t' Stack.t

val exec_fconverti :
  Link.Env.t' Stack.t -> Types.nn -> Types.nn -> Types.sx -> Link.Env.t' Stack.t

val exec_ireinterpretf :
  Link.Env.t' Stack.t -> Types.nn -> Types.nn -> Link.Env.t' Stack.t

val exec_freinterpreti :
  Link.Env.t' Stack.t -> Types.nn -> Types.nn -> Link.Env.t' Stack.t
