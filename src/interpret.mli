val module_ : Link.module_to_run -> (unit, string) result

val exec_vfunc :
     Link.Env.t' Stack.t
  -> Link.Env.t' Value.func
  -> (Link.Env.t' Stack.t, string) result
