(** Module to interpret a linked module. *)

(** interpret a module *)
val modul :
     Value.Func.extern_func Link.envs
  -> Value.Func.extern_func Link.module_to_run
  -> unit Result.t

(** interpret a function with a given input stack and produce a new stack*)
val exec_vfunc :
     Stack.t
  -> Value.Func.extern_func Link.envs
  -> Link.extern_func
  -> Value.Func.t
  -> Stack.t Result.t

val exec_iunop : Stack.t -> Types.nn -> Types.iunop -> Stack.t

val exec_funop : Stack.t -> Types.nn -> Types.funop -> Stack.t

val exec_ibinop : Stack.t -> Types.nn -> Types.ibinop -> Stack.t

val exec_fbinop : Stack.t -> Types.nn -> Types.fbinop -> Stack.t

val exec_itestop : Stack.t -> Types.nn -> Types.itestop -> Stack.t

val exec_irelop : Stack.t -> Types.nn -> Types.irelop -> Stack.t

val exec_frelop : Stack.t -> Types.nn -> Types.frelop -> Stack.t

val exec_itruncf : Stack.t -> Types.nn -> Types.nn -> Types.sx -> Stack.t

val exec_itruncsatf : Stack.t -> Types.nn -> Types.nn -> Types.sx -> Stack.t

val exec_fconverti : Stack.t -> Types.nn -> Types.nn -> Types.sx -> Stack.t

val exec_ireinterpretf : Stack.t -> Types.nn -> Types.nn -> Stack.t

val exec_freinterpreti : Stack.t -> Types.nn -> Types.nn -> Stack.t
