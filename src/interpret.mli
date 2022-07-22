type env =
  { modules : Simplify.module_ array
  ; registered_modules : (string, int) Hashtbl.t
  }

val exec_func : env -> int -> Types.indice Types.func -> Types.const list -> Stack.t

val exec_module : env -> int -> env
