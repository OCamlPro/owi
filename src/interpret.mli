(** Module to interpret a linked module. *)

(** interpret a module *)
val module_ : Link.module_to_run -> (unit, string) result

(** interpret a function with a given input stack and produce a new stack*)
val exec_vfunc :
     Link.Env.t' Stack.t
  -> Link.Env.t' Value.Func.t
  -> (Link.Env.t' Stack.t, string) result

val  exec_ibinop :
     Link.Env.t' Stack.t
  -> Types.nn
  -> Types.ibinop
  -> Link.Env.t' Stack.t

val  exec_fbinop :
      Link.Env.t' Stack.t
   -> Types.nn
   -> Types.fbinop
   -> Link.Env.t' Stack.t
